## Build parse tables from prepared grammar
when defined(profiler):
  import nimprof

import grammar, nfa, unicode, parser_types, symbol_set, bitset, minimizer
import std/[
      algorithm,
      options,
      hashes,
      deques,
      strformat,
      times,
      sequtils,
      strutils,
      sets as stdsets,
      tables as stdtables
    ]
{.warning[UnusedImport]: off.}

type
  BuildTablesError* = object of CatchableError

  Tables* = object
    parseTable*: BuildParseTable
    mainLexTable*: BuildLexTable

  # Helper for Thompson's construction
  PatchTargetKind = enum
    ptAdvance
    ptSplitLeft
    ptSplitRight

  PatchTarget = object
    stateId: uint32
    kind: PatchTargetKind

  Fragment = object
    startState: uint32
    outArrows: seq[PatchTarget] # Unresolved transitions pointing to "next"

  TokenConflictStatus* = object
    matchesPrefix*: bool
    doesMatchContinuation*: bool
    doesMatchValidContinuation*: bool
    doesMatchSeparators*: bool
    matchesSameString*: bool
    matchesDifferentString*: bool

  TokenConflictMap* = object
    n*: int
    statusMatrix*: seq[TokenConflictStatus]
    followingTokens*: seq[HashSet[uint32]]
    startingCharsByIndex*: seq[CharacterSet]
    followingCharsByIndex*: seq[CharacterSet]

  FirstSets* = stdtables.Table[GrammarSymbol, SymbolSet]
  FollowSets* = stdtables.Table[GrammarSymbol, SymbolSet]
  
  # Precomputed transitive closure data structures (tree-sitter algorithm)
  FollowSetInfo = object
    lookaheads: SymbolSet
    propagatesLookaheads: bool  # Whether to inherit from parent item's lookahead
  
  TransitiveClosureAddition = object
    variableIndex: int
    productionIndex: int
    followInfo: FollowSetInfo
  
  ClosurePrecomputation = object
    # For each non-terminal i, additions[i] contains all items that must be
    # added to an item set when non-terminal i appears as the next symbol
    additions: seq[seq[TransitiveClosureAddition]]

  # === New LALR(1) Closure Cache Types (using BitSet) ===
  
  LookaheadSet* = BitSet  # Alias for clarity - set of terminal symbols
  
  LookaheadSetInfo* = object
    ## Follow set information using BitSet for efficient operations.
    lookaheads*: LookaheadSet
    propagatesLookaheads*: bool  # Whether to inherit from parent item's lookahead
  
  ClosureAddition* = object
    ## Precomputed expansion: when we see non-terminal N at position,
    ## we must add these productions with these lookaheads.
    variableIndex*: uint16
    productionIndex*: uint16
    followInfo*: LookaheadSetInfo
  
  ClosureCache* = object
    ## Precomputed closure additions for each non-terminal.
    ## additions[i] = all productions to add when expanding non-terminal i.
    additions*: seq[seq[ClosureAddition]]
    symbolContext*: SymbolContext  # For symbol <-> bit mapping

  # === Pager's LALR Algorithm Types ===
  # Enabled with -d:usePagerLALR compile flag
  
  LR0Item* = object
    ## LR(0) item: production with dot position, no lookahead
    variableIndex*: uint16
    productionIndex*: uint16
    position*: uint16
    inheritedPrecedence*: int32
  
  PropagationLink* = object
    ## Lookahead propagation: from one item to another
    targetStateId*: int
    targetItem*: LR0Item
  
  LR0State* = object
    ## State in LR(0) automaton with Pager's lookahead data
    kernels*: seq[LR0Item]  # Core LR(0) items
    lookaheads*: stdtables.Table[LR0Item, LookaheadSet]  # Computed lookaheads
    spontaneous*: stdtables.Table[LR0Item, LookaheadSet]  # Spontaneously generated
    propagations*: stdtables.Table[LR0Item, seq[PropagationLink]]  # Where to propagate

# Forward declarations
proc buildLexicalNfa(lexicalGrammar: var LexicalGrammar)
proc dfaFromNfa(
    lexicalGrammar: LexicalGrammar,
    tokenConflictMap: TokenConflictMap,
    startConfigs: seq[seq[uint32]]
): tuple[table: BuildLexTable, startStateMap: seq[uint32]]
proc buildParseTable*(grammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar): BuildParseTable

# Conflict helper declarations
proc computeFirst*(grammar: SyntaxGrammar): FirstSets
proc computeLast(grammar: SyntaxGrammar): stdtables.Table[GrammarSymbol, SymbolSet]
proc computeFollowingTokens(
    syntaxGrammar: SyntaxGrammar, 
    lexicalGrammar: LexicalGrammar,
    firstSets: FirstSets,
    lastSets: stdtables.Table[GrammarSymbol, SymbolSet]
): seq[HashSet[uint32]]
proc newTokenConflictMap*(
    lexicalGrammar: LexicalGrammar,
    followingTokens: seq[HashSet[uint32]]
): TokenConflictMap

proc buildTables*(syntaxGrammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar): Tables =
  ## Main entry point for building parse and lexical tables from a grammar.
  ##
  ## This procedure orchestrates the entire table construction pipeline:
  ## 1. Build LR parse table from syntax grammar
  ## 2. Build NFA from lexical rules
  ## 3. Compute conflict information (FIRST/LAST sets, token conflicts)
  ## 4. Determine unique lexical configurations needed for each parse state
  ## 5. Convert NFA to DFA with multiple start states (one per config)
  ## 6. Apply state minimization to reduce parse table size
  ##
  ## The result contains both the parse table (for syntax analysis) and
  ## the lexical table (for tokenization).
  ##
  ## Example:
  ##   let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
  ##   let tables = buildTables(syntaxGrammar, lexicalGrammar)
  ##   # tables.parseTable contains LR parse actions
  ##   # tables.mainLexTable contains DFA states for lexing
  
  # 1. Build parse table from syntax grammar FIRST
  # We need this to determine which lexical states are needed
  # Internal: buildParseTable computes firstSets internally.
  # We might need them separately for conflicts.
  # 1. Build NFA for lexical grammar (Moved up to support extra detection)
  var mutableLexical = lexicalGrammar
  buildLexicalNfa(mutableLexical)
  
  # 2. Check for external tokens that match extra patterns
  var mutableSyntax = syntaxGrammar
  
  var extraTerminalIndices = stdsets.initHashSet[int]()
  for extra in mutableSyntax.extraSymbols:
    if extra.kind == stTerminal:
      extraTerminalIndices.incl(extra.index.int)
  
  if extraTerminalIndices.len > 0 and mutableSyntax.externalTokens.len > 0:
    for i, extToken in mutableSyntax.externalTokens:
      let sym = GrammarSymbol(kind: stExternal, index: i.uint16)
      
      # Skip if already marked as extra
      if sym in mutableSyntax.extraSymbols: continue
      
      var checkStr = extToken.name
      
      # Try to find corresponding internal rule for named externals
      # e.g. "line_break" -> "\n"
      for v in mutableSyntax.variables:
        if v.name == extToken.name:
           # Check if it resolves to a single string terminal
           if v.productions.len == 1 and v.productions[0].steps.len == 1:
              let stepSym = v.productions[0].steps[0].symbol
              if stepSym.kind == stTerminal:
                 let lvIdx = stepSym.index.int
                 if lvIdx >= 0 and lvIdx < mutableLexical.variables.len:
                    let lv: grammar.LexicalVariable = mutableLexical.variables[lvIdx]
                    let r = lv.rule
                    if r.kind == rkString:
                       checkStr = r.stringValue
           break
      
      if checkStr.len == 0: continue
      
      var cursor = newNfaCursor(addr mutableLexical.nfa)
      var matches = true
      
      for c in checkStr:
        let nextStates = cursor.advance(c, false)
        if nextStates.len == 0:
           matches = false
           break
           
      if matches:
         let completions = cursor.completions()
         var matchesExtra = false
         for (varIdx, _) in completions:
            if varIdx in extraTerminalIndices:
               matchesExtra = true
               break
         
         if matchesExtra:
            debugEchoMsg "[BuildTables] External token matches extra pattern: ", extToken.name.escape
            mutableSyntax.extraSymbols.add(sym)

  # 3. Build parse table from syntax grammar
  # Using modified syntax grammar with newly detected extras
  var parseTable = buildParseTable(mutableSyntax, mutableLexical)

  let firstSets = computeFirst(syntaxGrammar)
  let lastSets = computeLast(syntaxGrammar)
  let followingTokens = computeFollowingTokens(syntaxGrammar, mutableLexical, firstSets, lastSets)
  let tokenConflictMap = newTokenConflictMap(mutableLexical, followingTokens)
  
  # 4. Determine unique lexical configurations derived from Parse Table
  # Collect valid lookaheads for each parse state
  var startConfigs = newSeq[seq[uint32]]()
  var uniqueConfigs = stdtables.initTable[HashSet[uint32], uint32]()
  
  for i, entry in parseTable.entries:
    var requiredTokens = initHashSet[uint32]()
    
    # Collect terminals from Action Map
    for item in entry.actionMap:
       if item.sym.kind == stTerminal:
          # Terminal symbols correspond directly to lexical variables indices
          let varIdx = item.sym.index.int
          if varIdx >= 0 and varIdx < mutableLexical.variables.len:
             requiredTokens.incl(mutableLexical.variables[varIdx].startState)
     
    # Also include Extra symbols (whitespace, comments) derived from grammar
    for extra in syntaxGrammar.extraSymbols:
       if extra.kind == stTerminal:
         let varIdx = extra.index.int
         if varIdx >= 0 and varIdx < mutableLexical.variables.len:
           requiredTokens.incl(mutableLexical.variables[varIdx].startState)
    
    # Convert to sorted seq for canonical key
    var configSeq = newSeq[uint32]()
    for s in requiredTokens: configSeq.add(s)
    configSeq.sort()
    
    let configSet = toHashSet(configSeq) # Still use HashSet for uniqueness check map?
    # Or map seq directly? stdtables supports seq keys if elements support hash.
    # But startConfigs logic used HashSet previously.
    if configSet notin uniqueConfigs:
       uniqueConfigs[configSet] = uint32(startConfigs.len)
       startConfigs.add(configSeq)
    
    parseTable.entries[i].lexState = uniqueConfigs[configSet]
  
  # 5. Build DFA from NFA with multiple start configurations
  let (mainLexTable, startStateMap) = dfaFromNfa(mutableLexical, tokenConflictMap, startConfigs)
  
  # Map start states back to parse table if needed?
  # dfaFromNfa returns map: startConfig index -> DFA State ID.
  # We assigned startConfigs indices to entries.
  # Update parseTable entries with actual DFA State ID.
  
  for i in 0 ..< parseTable.entries.len:
      let configIdx = parseTable.entries[i].lexState
      if configIdx < uint32(startStateMap.len):
          parseTable.entries[i].lexState = startStateMap[configIdx.int]
  
  
  Tables(
    parseTable: parseTable,
    mainLexTable: mainLexTable
  )

# --- implementation details ---

proc patch(nfa: var Nfa, targets: seq[PatchTarget], nextState: uint32) =
  for target in targets:
    if target.stateId >= uint32(nfa.states.len):
      continue # Should not happen
    
    case target.kind
    of ptAdvance:
      nfa.states[target.stateId.int].advanceStateId = nextState
    of ptSplitLeft:
      nfa.states[target.stateId.int].splitLeftState = nextState
    of ptSplitRight:
      nfa.states[target.stateId.int].splitRightState = nextState

proc compileString(nfa: var Nfa, s: string): Fragment =
  if s.len == 0:
    # Empty string matches immediately
    # We need a no-op state that transitions to next?
    # Or just return a fragment that does nothing?
    # Actually invalid for rkString to be empty usually?
    # If empty, it's epsilon.
    # We can create a split? Or just return no states and start=next?
    # Easier: return a "dummy" fragment?
    # We will create one NFA state to represent "entry" that splits to next?
    # Let's say epsilon is just "immediate success".
    # But checking Thompson construction:
    # Epsilon is a state that transitions to next via epsilon-transition (split with one arm?).
    # `nfa.nim` nskSplit can act as epsilon transition (both same? or one dummy?)
    # or nskSplit with one branch is epsilon.
    # `nfa.nim`: state.kind nskAdvance consumes char.
    # nskSplit is epsilon.
    # Let's make an epsilon state: Split(next, next).
    let stateId = nfa.addState(NfaState(
      kind: nskSplit,
      splitLeftState: 0, # to patch
      splitRightState: 0 # to patch
    ))
    return Fragment(
      startState: stateId,
      outArrows: @[
        PatchTarget(stateId: stateId, kind: ptSplitLeft),
        PatchTarget(stateId: stateId, kind: ptSplitRight)
      ]
    )


  var firstState = uint32(0)
  var lastState = uint32(0)
  
  # Iterate runes
  let runes = toRunes(s)
  
  result.outArrows = @[]
  
  for idx, r in runes:
    let charSet = CharacterSet(ranges: @[CharacterRange(start: uint32(r), `end`: uint32(r) + 1)])
    # If unicode rune > 255, strict `char` cast is bad.
    # `nfa.nim` supports `CharacterRange` with `uint32`.
    # We should fix `fromChar` or use `fromRange` with `uint32` logic if possible.
    # nfa.nim: `proc fromChar*(c: char)`
    # We should add `fromRune` locally or fix nfa.
    # For now assume `s` is ASCII/UTF-8 bytes as chars loop?
    # `rkString` usually raw bytes sequences in tree-sitter.
    
    let stateId = nfa.addState(NfaState(
      kind: nskAdvance,
      advanceChars: charSet,
      advanceStateId: 0, # Pending patch
      advanceIsSep: false,
      advancePrecedence: 0
    ))
    
    if idx == 0:
      firstState = stateId
    else:
      # Patch previous to this
      nfa.states[lastState.int].advanceStateId = stateId
      
    lastState = stateId
    
  result.startState = firstState
  result.outArrows.add(PatchTarget(stateId: lastState, kind: ptAdvance))

# --- Regex Parsing ---

type
  RegexParser = object
    pattern: string
    pos: int

proc peek(p: RegexParser): char =
  if p.pos < p.pattern.len: p.pattern[p.pos] else: '\0'

proc next(p: var RegexParser): char =
  result = p.peek()
  if p.pos < p.pattern.len: inc p.pos

proc consume(p: var RegexParser, c: char): bool =
  if p.peek() == c:
    inc p.pos
    true
  else:
    false

proc parseRegexInternal(p: var RegexParser, nfa: var Nfa): Fragment

# Character Class Parsing
proc parseCharClass(p: var RegexParser, nfa: var Nfa): Fragment =
  # Expect '[' already consumed
  var negated = false
  if p.consume('^'):
    negated = true
  
  var ranges: seq[CharacterRange] = @[]
  
  while p.peek() != ']' and p.peek() != '\0':
    let c = p.next()
    if c == '\\':
        # Escape inside class
        let esc = p.next()
        case esc
        of 'd':
          # Digits 0-9
          ranges.add(CharacterRange(start: 48, `end`: 58))
        of 'w':
          # Word characters
          ranges.add(CharacterRange(start: 48, `end`: 58))
          ranges.add(CharacterRange(start: 65, `end`: 91))
          ranges.add(CharacterRange(start: 95, `end`: 96))
          ranges.add(CharacterRange(start: 97, `end`: 123))
        of 's':
          # Whitespace
          ranges.add(CharacterRange(start: 9, `end`: 11))
          ranges.add(CharacterRange(start: 13, `end`: 14))
          ranges.add(CharacterRange(start: 32, `end`: 33))
        else:
          let code = case esc
            of 'n': uint32(10)
            of 'r': uint32(13)
            of 't': uint32(9)
            of '\\': uint32(92)
            of '"': uint32(34)
            of ']': uint32(93)
            of '-': uint32(45)
            of '/': uint32(47)
            else: uint32(esc)
          ranges.add(CharacterRange(start: code, `end`: code + 1))
    elif p.peek() == '-':
        discard p.next() # skip -
        let endChar = p.next()
        # Add range c..endChar
        ranges.add(CharacterRange(start: uint32(c), `end`: uint32(endChar) + 1))
    else:
        ranges.add(CharacterRange(start: uint32(c), `end`: uint32(c) + 1))
  
  discard p.consume(']') # Consume closing bracket
  
  # Sort ranges by start point
  ranges.sort(proc (a, b: CharacterRange): int =
    cmp(a.start, b.start)
  )
  
  var charSet = CharacterSet(ranges: ranges)
  
  if negated:
    charSet = negate(charSet)
  
  let stateId = nfa.addState(NfaState(
      kind: nskAdvance,
      advanceChars: charSet,
      advanceStateId: 0,
      advanceIsSep: false,
      advancePrecedence: 0
  ))
  
  return Fragment(
      startState: stateId,
      outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)]
  )

proc parseAtom(p: var RegexParser, nfa: var Nfa): Fragment =
  let c = p.peek()
  case c
  of '(':
    discard p.next()
    result = parseRegexInternal(p, nfa)
    discard p.consume(')')
  of '[':
    discard p.next()
    result = parseCharClass(p, nfa)
  of '\\':
    discard p.next()
    let esc = p.next()
    var charSet: CharacterSet
    case esc
    of 'd':
      # Digits 0-9
      let ranges = @[CharacterRange(start: 48, `end`: 58)]
      charSet = CharacterSet(ranges: ranges)
    of 'w':
      # Word characters: a-z, A-Z, 0-9, _
      let ranges = @[
        CharacterRange(start: 48, `end`: 58),  # 0-9
        CharacterRange(start: 65, `end`: 91),  # A-Z
        CharacterRange(start: 95, `end`: 96),  # _
        CharacterRange(start: 97, `end`: 123)  # a-z
      ]
      charSet = CharacterSet(ranges: ranges)
    of 's':
      # Whitespace: \t, \n, \r, space
      let ranges = @[
        CharacterRange(start: 9, `end`: 11),   # \t, \n
        CharacterRange(start: 13, `end`: 14),  # \r
        CharacterRange(start: 32, `end`: 33)   # space
      ]
      charSet = CharacterSet(ranges: ranges)
    of 'n': charSet = fromChar(10.char)
    of 'r': charSet = fromChar(13.char)
    of 't': charSet = fromChar(9.char)
    of '\\': charSet = fromChar('\\')
    of '"': charSet = fromChar('"')
    of '\'': charSet = fromChar('\'')
    of '/': charSet = fromChar('/')
    else: charSet = fromChar(esc)

    let stateId = nfa.addState(NfaState(
        kind: nskAdvance,
        advanceChars: charSet,
        advanceStateId: 0
    ))
    result = Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])
  of '.', '|', '*', '+', '?', ')':
    # Should not happen in atom position unless syntax error or done
    # Dot is special, means any char (except newline usually)
     if c == '.':
         # Wildcard (match any character except newline)
         discard p.next()
         # range 0..Max excluding 10 (\n)
         let ranges = @[
           CharacterRange(start: 0, `end`: 10),
           CharacterRange(start: 11, `end`: EndChar)
         ]
         let stateId = nfa.addState(NfaState(
             kind: nskAdvance,
             advanceChars: CharacterSet(ranges: ranges),
             advanceStateId: 0
         ))
         result = Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])
     else:
         # Unexpected char or empty
         # Create epsilon?
         raise newException(BuildTablesError, "Unexpected char in regex: " & c)
  else:
    # Literal char
    discard p.next()
    let charSet = fromChar(c)
    let stateId = nfa.addState(NfaState(
        kind: nskAdvance,
        advanceChars: charSet,
        advanceStateId: 0
    ))
    result = Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])

proc parseQuantifier(p: var RegexParser, nfa: var Nfa, atom: Fragment): Fragment =
  let c = p.peek()
  case c
  of '*':
    discard p.next()
    # Zero or more: Split(atomStart, next) -> atom -> Split(atomStart, next)
    # Actually:
    # start = Split(atomStart, end)
    # patch atom.out back to start
    # out = start.splitRight (the 'next' branch)
    
    let splitId = nfa.addState(NfaState(
        kind: nskSplit,
        splitLeftState: atom.startState,
        splitRightState: 0 # pending
    ))
    patch(nfa, atom.outArrows, splitId)
    return Fragment(
        startState: splitId,
        outArrows: @[PatchTarget(stateId: splitId, kind: ptSplitRight)]
    )
  of '+':
    discard p.next()
    # One or more: atom -> Split(atomStart, next)
    let splitId = nfa.addState(NfaState(
        kind: nskSplit,
        splitLeftState: atom.startState,
        splitRightState: 0 # pending
    ))
    patch(nfa, atom.outArrows, splitId)
    return Fragment(
        startState: atom.startState,
        outArrows: @[PatchTarget(stateId: splitId, kind: ptSplitRight)]
    )
  of '?':
    discard p.next()
    # Zero or one: Split(atomStart, next) -> atom -> next
    let splitId = nfa.addState(NfaState(
        kind: nskSplit,
        splitLeftState: atom.startState,
        splitRightState: 0
    ))
    var outs = atom.outArrows
    outs.add(PatchTarget(stateId: splitId, kind: ptSplitRight))
    return Fragment(startState: splitId, outArrows: outs)
  else:
    return atom

proc parseSequence(p: var RegexParser, nfa: var Nfa): Fragment =
  # Sequence of atoms (with optional quantifiers)
  # Loop until | or ) or end
  
  # Note: A sequence might be empty
  if p.peek() == '|' or p.peek() == ')' or p.peek() == '\0':
      # Empty matches epsilon
      let stateId = nfa.addState(NfaState(kind: nskSplit, splitLeftState: 0, splitRightState: 0))
      return Fragment(startState: stateId, outArrows: @[
          PatchTarget(stateId: stateId, kind: ptSplitLeft),
          PatchTarget(stateId: stateId, kind: ptSplitRight)
      ])

  var currentFrag = parseQuantifier(p, nfa, parseAtom(p, nfa))
  
  while p.peek() != '|' and p.peek() != ')' and p.peek() != '\0':
      let nextFrag = parseQuantifier(p, nfa, parseAtom(p, nfa))
      patch(nfa, currentFrag.outArrows, nextFrag.startState)
      currentFrag.outArrows = nextFrag.outArrows
      # startState unchanged (sequence start)
      
  return currentFrag

proc parseRegexInternal(p: var RegexParser, nfa: var Nfa): Fragment =
  # Alternations: Seq | Seq | ...
  let firstSeq = parseSequence(p, nfa)
  
  if p.peek() != '|':
      return firstSeq
      
  # Handle alternation
  var currentStart = firstSeq.startState
  var currentOut = firstSeq.outArrows
  
  while p.consume('|'):
      let nextSeq = parseSequence(p, nfa)
      let splitId = nfa.addState(NfaState(
          kind: nskSplit,
          splitLeftState: currentStart,
          splitRightState: nextSeq.startState
      ))
      currentStart = splitId
      currentOut.add(nextSeq.outArrows)
      
  return Fragment(startState: currentStart, outArrows: currentOut)

proc compileRegex(nfa: var Nfa, pattern: string): Fragment =
  try:
    var p = RegexParser(pattern: pattern, pos: 0)
    parseRegexInternal(p, nfa)
  except BuildTablesError:
    # Unsupported regex feature - fall back to letter-matching pattern
    echo "[Treestand] Warning: Unsupported regex pattern '", pattern, "' - using [a-zA-Z] fallback"
    # Create a pattern that matches letters only (not operators or punctuation)
    let stateId = nfa.addState(NfaState(
        kind: nskAdvance,
        advanceChars: CharacterSet(ranges: @[
          CharacterRange(start: 65, `end`: 91),   # A-Z
          CharacterRange(start: 97, `end`: 123)   # a-z
        ]),
        advanceStateId: 0
    ))
    return Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])

proc compileRule(nfa: var Nfa, rule: Rule): Fragment =
  case rule.kind
  of rkString:
    return compileString(nfa, rule.stringValue)
  of rkPattern:
     return compileRegex(nfa, rule.patternValue)
  of rkSeq:
    # A then B
    # start = compile(A)
    # patch(A.out, start(B))
    # out = B.out
    
    # Just generic seq
    if rule.seqMembers.len == 0:
        return compileString(nfa, "")
        
    var frag = compileRule(nfa, rule.seqMembers[0])
    for i in 1 ..< rule.seqMembers.len:
      let nextFrag = compileRule(nfa, rule.seqMembers[i])
      patch(nfa, frag.outArrows, nextFrag.startState)
      frag.outArrows = nextFrag.outArrows
      # startState remains initial
    return frag
    
  of rkChoice:
    # A or B
    # Split(startA, startB)
    # out = outA + outB
    
    if rule.choiceMembers.len == 0:
        return compileString(nfa, "") # Empty choice??
    
    # We can chain splits for multiple choices.
    # Split(A, Split(B, C...))
    
    let firstFrag = compileRule(nfa, rule.choiceMembers[0])
    var currentStart = firstFrag.startState
    var currentOut = firstFrag.outArrows
    
    for i in 1 ..< rule.choiceMembers.len:
        let nextFrag = compileRule(nfa, rule.choiceMembers[i])
        let splitId = nfa.addState(NfaState(
            kind: nskSplit,
            splitLeftState: currentStart,
            splitRightState: nextFrag.startState
        ))
        currentStart = splitId
        currentOut.add(nextFrag.outArrows)
        
    return Fragment(startState: currentStart, outArrows: currentOut)
    
  of rkMetadata:
    return compileRule(nfa, rule.metadataRule[])
  of rkBlank:
    return compileString(nfa, "")
  of rkReserved:
    return compileRule(nfa, rule.reservedRule[])
  of rkRepeat:
    # Zero or more: Split(atomStart, next) -> atom -> Split(atomStart, next)
    let atom = compileRule(nfa, rule.repeatContent[])
    
    let splitId = nfa.addState(NfaState(
        kind: nskSplit,
        splitLeftState: atom.startState,
        splitRightState: 0 # pending
    ))
    patch(nfa, atom.outArrows, splitId)
    return Fragment(
        startState: splitId,
        outArrows: @[PatchTarget(stateId: splitId, kind: ptSplitRight)]
    )
    
  else:
    # Fallback / TODO
    # Throw error or returns dummy
    # For test, we might encounter patterns?
    if rule.kind == rkPattern:
        # For now, treat as single "any" char just to pass basic structure test if needed?
        # Or error.
        raise newException(BuildTablesError, "Pattern not implemented in NFA build yet")
    else:
        raise newException(BuildTablesError, "Unsupported rule kind in lexical grammar: " & $rule.kind)

proc buildLexicalNfa(lexicalGrammar: var LexicalGrammar) =
  var nfaObj = newNfa()
  
  for i in 0 ..< lexicalGrammar.variables.len:
    let variable = lexicalGrammar.variables[i]
    let fragment = compileRule(nfaObj, variable.rule)
    
    # Set start state in variable
    lexicalGrammar.variables[i].startState = fragment.startState
    
    # Connect fragment out arrows to an Accept state
    let acceptId = nfaObj.addState(NfaState(
      kind: nskAccept,
      acceptVariableIndex: i,
      acceptPrecedence: variable.implicitPrecedence
    ))
    
    patch(nfaObj, fragment.outArrows, acceptId)
    
  lexicalGrammar.nfa = nfaObj

# --- Conflict Detection Helpers ---
proc computeNullability*(grammar: var SyntaxGrammar) =
  ## Analyzes the grammar and sets the .isNullable flag on all variables
  var changed = true

  # 1. Initialize: Default to false (already false by default in Nim, but being explicit)
  for i in 0 ..< grammar.variables.len:
    grammar.variables[i].isNullable = false

  # 2. Iterate until fixpoint (standard algorithm)
  while changed:
    changed = false
    for i in 0 ..< grammar.variables.len:
      # If already nullable, skip
      if grammar.variables[i].isNullable: continue

      # Check all productions
      for production in grammar.variables[i].productions:
        # Empty production ([]) means strictly nullable
        if production.steps.len == 0:
          grammar.variables[i].isNullable = true
          changed = true
          break

        # Check if all children are nullable
        var allChildrenNullable = true
        for step in production.steps:
          if step.symbol.kind == stTerminal or step.symbol.kind == stExternal:
            # Terminals and externals (tokens) are never nullable
            allChildrenNullable = false
            break
          
          if step.symbol.kind == stNonTerminal:
             # Check the flag on the referenced variable
             if not grammar.variables[step.symbol.index].isNullable:
               allChildrenNullable = false
               break
        
        if allChildrenNullable:
           grammar.variables[i].isNullable = true
           changed = true
           break

proc isNullable(grammar: SyntaxGrammar, symbol: GrammarSymbol): bool =
  # Only Non-Terminals can be nullable
  if symbol.kind != stNonTerminal:
    return false
  
  # O(1) Lookup
  return grammar.variables[symbol.index].isNullable

proc computeLast(grammar: SyntaxGrammar): stdtables.Table[GrammarSymbol, SymbolSet] =
  ## Compute LAST sets for all non-terminals (similar to FIRST but from end)
  result = stdtables.initTable[GrammarSymbol, SymbolSet]()
  var changed = true
  
  # Initialize
  for i in 0 ..< grammar.variables.len:
    let sym = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
    result[sym] = initSymbolSet(8)  # Average LAST set size
  
  # Iterate until fixpoint
  while changed:
    changed = false
    
    for i in 0 ..< grammar.variables.len:
      let lhs = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
      let variable = grammar.variables[i]
      let oldSize = result[lhs].len
      
      for production in variable.productions:
        if production.steps.len == 0:
          continue
          
        # Process steps in reverse
        for j in countdown(production.steps.len - 1, 0):
          let step = production.steps[j]
          let sym = step.symbol
          
          if sym.kind == stNonTerminal:
            # Add LAST(symbol) to LAST(lhs)
            if sym in result:
              for lastSym in result[sym]:
                result[lhs].incl(lastSym)
          else:
            # Terminal: add to LAST(lhs)
            result[lhs].incl(sym)
          
          # If this symbol is not nullable, stop
          if not isNullable(grammar, sym):
            break
            
      if result[lhs].len != oldSize:
        changed = true

proc computeFollowingTokens(
    syntaxGrammar: SyntaxGrammar, 
    lexicalGrammar: LexicalGrammar,
    firstSets: FirstSets,
    lastSets: stdtables.Table[GrammarSymbol, SymbolSet]
): seq[HashSet[uint32]] =
  ## Compute which lexical tokens can follow other lexical tokens
  ## result[tokenIndex] = Set of tokens that can follow 'tokenIndex'
  
  var resultSeq = newSeq[HashSet[uint32]](lexicalGrammar.variables.len)
  for i in 0 ..< resultSeq.len: resultSeq[i] = initHashSet[uint32]()
  
  # Initialize with full set? No. Empty.
  
  # We iterate all productions to find sequences ... A B ...
  # Tokens in LAST(A) can be followed by FIRST(B)
  
  for variable in syntaxGrammar.variables:
    for production in variable.productions:
      for i in 0 ..< production.steps.len - 1:
        let leftSym = production.steps[i].symbol
        let rightSym = production.steps[i + 1].symbol
        
        # Collect tokens that can end 'leftSym'
        var leftTokens = initSymbolSet()
        if leftSym.kind == stTerminal:
          leftTokens.incl(leftSym)
        elif leftSym.kind == stNonTerminal:
          if leftSym in lastSets:
             leftTokens = lastSets[leftSym]
        
        # Collect tokens that can start 'rightSym'
        var rightTokens = initSymbolSet()
        if rightSym.kind == stTerminal:
          rightTokens.incl(rightSym)
        elif rightSym.kind == stNonTerminal:
          if rightSym in firstSets:
             rightTokens = firstSets[rightSym]
             
        # Add relation
        for l in leftTokens:
          if l.kind == stTerminal:
            for r in rightTokens:
              if r.kind == stTerminal:
                 # Map terminal index to lexical variable index
                 # Assuming 1-to-1 mapping for stTerminal
                 if l.index.int >= 0 and l.index.int < resultSeq.len and r.index.int >= 0 and r.index.int < resultSeq.len:
                    resultSeq[l.index.int].incl(uint32(r.index.int))
  
  # Handle extra symbols (can follow anything?)
  # In tree-sitter, extra symbols are added to 'following_tokens' for every token?
  # Or just allowed everywhere?
  # Tree-sitter: `result[extra.index] = all_tokens.clone()` and `entry.insert(*extra)` for all entries.
  
  let allTokensCount = lexicalGrammar.variables.len
  for extra in syntaxGrammar.extraSymbols:
      if extra.kind == stTerminal:
          # Extra can follow any token
          for i in 0 ..< resultSeq.len:
              if extra.index.int >= 0 and extra.index.int < resultSeq.len:
                  resultSeq[i].incl(uint32(extra.index.int))
          
          # Any token can follow Extra
          if extra.index.int >= 0 and extra.index.int < resultSeq.len:
             for j in 0 ..< allTokensCount:
                 resultSeq[extra.index].incl(uint32(j))
                 
  resultSeq

proc matrixIndex(n, i, j: int): int =
  n * i + j

# Forward declarations for conflict helper procs
proc computeConflictStatus(
    cursor: var NfaCursor,
    grammar: LexicalGrammar,
    followingChars: seq[CharacterSet],
    i, j: int
): (TokenConflictStatus, TokenConflictStatus) =
  var status1 = TokenConflictStatus()
  var status2 = TokenConflictStatus()
  
  let startState1 = grammar.variables[i].startState
  let startState2 = grammar.variables[j].startState
  
  var visited = initHashSet[(seq[uint32], seq[uint32])]()
  var queue = newSeq[(seq[uint32], seq[uint32], bool)]() # states1, states2, isSep
  
  # Using simple seq as queue. For efficiency Deque is better but seq is ok for small NFA.
  
  # Initial state
  var c1 = cursor # Copy
  setStates(c1, @[startState1])
  var states1 = c1.stateIds
  
  var c2 = cursor
  setStates(c2, @[startState2])
  var states2 = c2.stateIds
  
  queue.add((states1, states2, false))
  visited.incl((states1, states2))
  
  var qIdx = 0
  while qIdx < queue.len:
    let (s1, s2, isSep) = queue[qIdx]
    qIdx += 1
    
    setStates(c1, s1)
    setStates(c2, s2)
    
    # Check accept states
    var prec1: Option[int32]
    var prec2: Option[int32]
    
    for (id, prec) in completions(c1):
        if id == i: prec1 = some(prec)
    for (id, prec) in completions(c2):
        if id == j: prec2 = some(prec)
        
    if prec1.isSome and prec2.isSome:
        status1.matchesSameString = true
        status2.matchesSameString = true
    elif prec1.isSome:
        status1.matchesPrefix = true
        # Check continuations for 2
        # If 2 continues with valid following chars of 1?
        # Tree-sitter: checks if 2 continues with characters that are valid *following* characters of 1.
        let follow1 = followingChars[i]
        
        # Check transitions of 2
        let trans2 = transitionChars(c2)
        for (chars, sep) in trans2:
             if sep:
                 status2.doesMatchSeparators = true
             # Check overlap with follow1
             # If chars overlap follow1 => valid continuation
             for r in chars.ranges:
                 # Inefficient usage: create temp set?
                 # Or helper `intersects`
                 var rangeSet = emptyCharacterSet() 
                 rangeSet.ranges.add(r)
                 # Wait, CharacterSet.contains or intersection logic needed.
                 # Assuming simple intersection check:
                 # For now, just check ranges overlap.
                 for fr in follow1.ranges:
                     if max(r.start, fr.start) < min(r.`end`, fr.`end`):
                         status2.doesMatchValidContinuation = true
                     elif sep: # If separator transition 
                         status2.doesMatchValidContinuation = true # Separator always valid continuation?
                         
        status2.doesMatchContinuation = true

    elif prec2.isSome:
        status2.matchesPrefix = true
        let follow2 = followingChars[j]
        let trans1 = transitionChars(c1)
        for (chars, sep) in trans1:
             if sep:
                 status1.doesMatchSeparators = true
             for r in chars.ranges:
                 for fr in follow2.ranges:
                     if max(r.start, fr.start) < min(r.`end`, fr.`end`):
                         status1.doesMatchValidContinuation = true
                     elif sep:
                         status1.doesMatchValidContinuation = true

        status1.doesMatchContinuation = true
            # Advance
    # Find common transitions - cache these outside inner loop
    let trans1 = transitions(c1)
    let trans2 = transitions(c2)
    
    for t1 in trans1:
        for t2 in trans2:
            if t1.isSeparator == t2.isSeparator:
                 # Use optimized intersects helper - much faster than nested loops
                 if intersects(t1.characters, t2.characters):
                     if not (t1.states.len == 0 and t2.states.len == 0): # Avoid empty?
                         let nextStates1 = t1.states
                         let nextStates2 = t2.states
                         
                         if not visited.contains((nextStates1, nextStates2)):
                             visited.incl((nextStates1, nextStates2))
                             queue.add((nextStates1, nextStates2, t1.isSeparator or isSep))
                             
    # If different string matched?
    # If we reached a state where sets are disjoint?
    # Actually `matchesDifferentString` logic: ??? 
    # Tree-sitter: `matches_different_string` default false?
    # It seems tree-sitter doesn't explicitly compute `matches_different_string` in `compute_conflict_status`.
    # It is derived?
    # Wait, `TokenConflictStatus` has `matches_different_string`.
    # In `token_conflicts.rs`, it sets it?
    # `let has_common_transition = ...`
    # If !has_common_transition and they are not both accepting?
    
  (status1, status2)

proc getStartingChars(cursor: var NfaCursor, grammar: LexicalGrammar): seq[CharacterSet] =
  result = newSeq[CharacterSet](grammar.variables.len)
  var workCursor = cursor # Copy cursor
  
  for i in 0 ..< grammar.variables.len:
    let variable = grammar.variables[i]
    reset(workCursor)
    setStates(workCursor, @[variable.startState])
    
    var allChars = emptyCharacterSet()
    # Iterate transitions
    let trans = transitionChars(workCursor)
    for (chars, _) in trans:
      # efficient union needed?
      # basic merge
      for r in chars.ranges:
        allChars.ranges.add(r)
    # Simplify/merge ranges? 
    # For now just store raw ranges. CharacterSet should support better `add`?
    # nfa.nim definition of CharacterSet is simple.
    result[i] = allChars

proc getFollowingChars(
    startingChars: seq[CharacterSet],
    followingTokens: seq[HashSet[uint32]]
): seq[CharacterSet] =
    result = newSeq[CharacterSet](followingTokens.len)
    for i, followers in followingTokens:
        var chars = emptyCharacterSet()
        for tokenIdx in followers:
            if tokenIdx < uint32(startingChars.len):
                let sc = startingChars[tokenIdx.int]
                for r in sc.ranges:
                    chars.ranges.add(r)
        result[i] = chars

proc newTokenConflictMap*(lexicalGrammar: LexicalGrammar, followingTokens: seq[HashSet[uint32]]): TokenConflictMap =
    let n = lexicalGrammar.variables.len
    var cursor = newNfaCursor(unsafeAddr lexicalGrammar.nfa) # Assuming nfa is addressable? nfa is field of grammar.
    # But grammar is value. `unsafeAddr` might be dangerous if grammar moves.
    # dfaFromNfa callers should keep grammar stable.
    
    let startingChars = getStartingChars(cursor, lexicalGrammar)
    let followingChars = getFollowingChars(startingChars, followingTokens)
    
    var statusMatrix = newSeq[TokenConflictStatus](n * n)
    
    for i in 0 ..< n:
        for j in 0 ..< i:
             let (status1, status2) = computeConflictStatus(cursor, lexicalGrammar, followingChars, i, j)
             statusMatrix[matrixIndex(n, i, j)] = status1
             statusMatrix[matrixIndex(n, j, i)] = status2
             
    TokenConflictMap(
        n: n,
        statusMatrix: statusMatrix,
        followingTokens: followingTokens,
        startingCharsByIndex: startingChars,
        followingCharsByIndex: followingChars
    )

proc doesConflict*(map: TokenConflictMap, i, j: int): bool =
    let entry = map.statusMatrix[matrixIndex(map.n, i, j)]
    entry.doesMatchValidContinuation or entry.doesMatchSeparators or entry.matchesSameString

proc doesMatchSameString*(map: TokenConflictMap, i, j: int): bool =
    map.statusMatrix[matrixIndex(map.n, i, j)].matchesSameString

proc preferToken*(grammar: LexicalGrammar, left: (int32, int), right: (int32, int)): bool =
    let (precLeft, idLeft) = left
    let (precRight, idRight) = right
    
    if precLeft < precRight: return false
    if precLeft > precRight: return true
    
    # Equal precedence
    if grammar.variables[idLeft].implicitPrecedence < grammar.variables[idRight].implicitPrecedence:
        return false
    if grammar.variables[idLeft].implicitPrecedence > grammar.variables[idRight].implicitPrecedence:
        return true
        
    # Same implicit precedence: prefer lower index (earlier definition)
    idLeft < idRight

proc preferTransition*(
    grammar: LexicalGrammar,
    t: NfaTransition,
    completedId: int,
    completedPrecedence: int32,
    hasSeparatorTransitions: bool
): bool =
    if t.precedence < completedPrecedence:
        return false
    if t.precedence == completedPrecedence:
        if t.isSeparator:
            return false
        # If separator transitions match, check if we are still matching the SAME token?
        # Tree-sitter logic:
        # if has_separator_transitions && !grammar.variable_indices_for_nfa_states(&t.states).any(|i| i == completed_id)
        #   return false
        
        # We need `variableIndicesForNfaStates` helper?
        # For now, simplify or assume false if ambiguous?
        # Let's check `nfa.nim` for variable mapping.
        # NFA states don't know which variable they belong to easily unless we track it?
        # Actually `nskAccept` has `acceptVariableIndex`.
        # Intermediate states don't.
        # Tree-sitter likely maps states to variables via graph analysis or construction metadata.
        # NfaCursor wraps `nfa` which has `states`.
        # `variable_indices_for_nfa_states` in Rust:
        # It iterates states and checks if they can reach accept states for variables? or are part of variables?
        # In `tree-sitter`, variables are separate NFA components usually.
        # But we built one big NFA.
        # We might need to approximate this or just return true for now.
        if hasSeparatorTransitions:
             return false # Conservative approximation?
    true

# --- DFA Construction ---

proc epsilonClosure(nfa: Nfa, states: HashSet[uint32]): HashSet[uint32] =
  result = states
  var stack = newSeq[uint32]()
  for s in states: stack.add(s)
  
  while stack.len > 0:
    let s = stack.pop()
    if s >= uint32(nfa.states.len): continue
    
    let state = nfa.states[s.int]
    case state.kind
    of nskSplit:
      if state.splitLeftState notin result:
        result.incl(state.splitLeftState)
        stack.add(state.splitLeftState)
      if state.splitRightState notin result:
        result.incl(state.splitRightState)
        stack.add(state.splitRightState)
    else:
      discard

      discard

proc dfaFromNfa(
    lexicalGrammar: LexicalGrammar,
    tokenConflictMap: TokenConflictMap,
    startConfigs: seq[seq[uint32]]
): tuple[table: BuildLexTable, startStateMap: seq[uint32]] =
  var dfaStates = newSeq[BuildLexState]()
  # Use sorted seq state key
  var stateMap = stdtables.initTable[seq[uint32], uint32]()
  var workList = initDeque[seq[uint32]]()  # Use Deque for O(1) operations
  var startStateMap = newSeq[uint32]()
  
  var cursor = newNfaCursor(unsafeAddr lexicalGrammar.nfa)

  proc getOrAddState(nfaStates: seq[uint32]): uint32 =
    # Caller should ensure nfaStates is sorted and unique (canonical)
    var sorted = nfaStates
    sorted.sort()
    
    if sorted in stateMap:
      return stateMap[sorted]
      
    let id = uint32(dfaStates.len)
    stateMap[sorted] = id
    dfaStates.add(BuildLexState(transitions: @[], accept: none(int)))
    workList.addLast(sorted)  # Use addLast instead of add
    return id

  for config in startConfigs:
    let initialSet = epsilonClosure(lexicalGrammar.nfa, toHashSet(config))
    var seqSet = newSeq[uint32]()
    for s in initialSet: seqSet.add(s)
    # Sort now
    seqSet.sort()
    startStateMap.add(getOrAddState(seqSet))
    
  
  while workList.len > 0:
     let currentStates = workList.popFirst()  # O(1) operation with Deque
     let currentStateId = stateMap[currentStates]
     
     setStates(cursor, currentStates)
     
     # Acceptance Logic
     var bestCompletion: Option[(int, int32)]
     for (id, prec) in completions(cursor):
        if bestCompletion.isSome:
           let (prevId, prevPrec) = bestCompletion.get
           if preferToken(lexicalGrammar, (prevPrec, prevId), (prec, id)):
              # Keep prev
              discard
           else:
              bestCompletion = some((id, prec))
        else:
           bestCompletion = some((id, prec))
           
     if bestCompletion.isSome:
        let (id, _) = bestCompletion.get
        dfaStates[currentStateId.int].accept = some(id)
        
     # Transitions Logic
     let transitions = transitions(cursor)
     var hasSep = false
     # Check separator transitions using transitionChars helper
     # transitionChars usage: seq[(CharacterSet, bool)]
     for (chars, sep) in transitionChars(cursor):
         if sep: 
           hasSep = true
           break
     
     for t in transitions:
        if bestCompletion.isSome:
           let (compId, compPrec) = bestCompletion.get
           if not preferTransition(lexicalGrammar, t, compId, compPrec, hasSep):
              continue
              
        # Find next state
        # t.states IS ALREADY THE NEXT STATE SET (computed by NfaCursor logic)
        # NfaCursor.transitions logic computes closure of next states.
        # But we must ensure it is converted to sorted seq.
        
        let nextStateId = getOrAddState(t.states)
        
        dfaStates[currentStateId.int].transitions.add(BuildLexTransition(
           characters: t.characters,
           isSeparator: t.isSeparator,
           precedence: t.precedence,
           state: nextStateId
        ))
        
  (BuildLexTable(states: dfaStates), startStateMap)

# --- LR Parse Table Construction ---
# FirstSets/isNullable moved up

proc computeFirst*(grammar: SyntaxGrammar): FirstSets =
  ## Computes FIRST sets for all non-terminals in the grammar.
  ##
  ## FIRST(A) = set of terminal symbols that can appear at the start
  ## of strings derivable from non-terminal A.
  ##
  ## Algorithm (standard fixpoint iteration):
  ## 1. Initialize FIRST(A) = {} for all non-terminals A
  ## 2. For each production A -> α₁ α₂ ... αₙ:
  ##    - Add FIRST(α₁) to FIRST(A)
  ##    - If α₁ is nullable, add FIRST(α₂), and so on
  ## 3. Repeat until no changes (fixpoint)
  ##
  ## Complexity: O(n * p) where n = number of non-terminals, p = productions
  ##
  ## Used for:
  ## - LR lookahead computation
  ## - Conflict resolution
  ## - Lexical state determination
  result = stdtables.initTable[GrammarSymbol, SymbolSet]()
  var changed = true
  
  # Initialize
  for i in 0 ..< grammar.variables.len:
    let sym = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
    result[sym] = initSymbolSet(8)  # Average FIRST set size
  
  # Iterate until fixpoint
  while changed:
    changed = false
    
    for i in 0 ..< grammar.variables.len:
      let lhs = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
      let variable = grammar.variables[i]
      let oldSize = result[lhs].len
      
      for production in variable.productions:
        if production.steps.len == 0:
          # Empty production contributes nothing to FIRST
          continue
          
        # Process each step in sequence
        for step in production.steps:
          let sym = step.symbol
          
          if sym.kind == stNonTerminal:
            # Add FIRST(symbol) to FIRST(lhs)
            if sym in result:
              for firstSym in result[sym]:
                result[lhs].incl(firstSym)
          else:
            # Terminal: add to FIRST(lhs)
            result[lhs].incl(sym)
          
          # If this symbol is not nullable, stop
          if not isNullable(grammar, sym):
            break
      
      if result[lhs].len != oldSize:
        changed = true

proc computeFollow*(grammar: SyntaxGrammar, firstSets: FirstSets): FollowSets =
  ## Compute FOLLOW sets for all non-terminals
  result = stdtables.initTable[GrammarSymbol, SymbolSet]()
  var changed = true
  
  # Initialize
  for i in 0 ..< grammar.variables.len:
    let sym = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
    result[sym] = initSymbolSet(8)  # Average FOLLOW set size
  
  # Add $ (end marker) to FOLLOW of start symbol (index 0)
  let endSym = GrammarSymbol(kind: stEnd, index: 0.uint16)
  result[GrammarSymbol(kind: stNonTerminal, index: 0.uint16)].incl(endSym)
  
  # Iterate until fixpoint
  while changed:
    changed = false
    
    for i in 0 ..< grammar.variables.len:
      let lhs = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
      let variable = grammar.variables[i]
      
      for production in variable.productions:
        # Process each step to compute FOLLOW sets
        for stepIdx in 0 ..< production.steps.len:
          let step = production.steps[stepIdx]
          let sym = step.symbol
          
          if sym.kind != stNonTerminal:
            continue  # Only compute FOLLOW for non-terminals
          
          let oldSize = result[sym].len
          
          # Check what comes after this symbol
          var allNullable = true
          for nextIdx in (stepIdx + 1) ..< production.steps.len:
            let nextSym = production.steps[nextIdx].symbol
            
            if nextSym.kind == stNonTerminal:
              # Add FIRST(nextSym) to FOLLOW(sym)
              if nextSym in firstSets:
                for firstSym in firstSets[nextSym]:
                  result[sym].incl(firstSym)
            else:
              # Terminal: add to FOLLOW(sym)
              result[sym].incl(nextSym)
            
            # Check if nextSym is nullable
            if not isNullable(grammar, nextSym):
              allNullable = false
              break
          
          # If all symbols after are nullable (or there are none), 
          # add FOLLOW(lhs) to FOLLOW(sym)
          if allNullable:
            if lhs in result:
              for followSym in result[lhs]:
                result[sym].incl(followSym)
          
          if result[sym].len != oldSize:
            changed = true

# --- LR(1) Items and Canonical Collection ---

type
  LR1Item* = object
    variableIndex*: int32         # Which variable (non-terminal) this production belongs to
    productionIndex*: int32       # Which production of that variable
    position*: int32              # Dot position (0 = before first symbol)
    lookahead*: GrammarSymbol     # Lookahead symbol
    inheritedPrecedence*: int32   # Precedence inherited from parent rule

proc hash*(item: LR1Item): Hash =
  when nimvm:
    for fld, val in item.fieldPairs:
      result = result !& hash(val)
  else:
    result = hash(cast[int](item)) # 64 bits
    result = result !& hash(cast[ptr int](item.position.addr)[]) # 64 bits
    result = result !& hash(item.inheritedPrecedence) # 32 bits

proc `==`*(a, b: LR1Item): bool =
  a.variableIndex == b.variableIndex and
  a.productionIndex == b.productionIndex and
  a.position == b.position and
  a.lookahead == b.lookahead and
  a.inheritedPrecedence == b.inheritedPrecedence

proc `<`*(a, b: LR1Item): bool =
  if a.variableIndex != b.variableIndex: return a.variableIndex < b.variableIndex
  if a.productionIndex != b.productionIndex: return a.productionIndex < b.productionIndex
  if a.position != b.position: return a.position < b.position
  if a.inheritedPrecedence != b.inheritedPrecedence: return a.inheritedPrecedence < b.inheritedPrecedence
  if a.lookahead.kind != b.lookahead.kind: return a.lookahead.kind < b.lookahead.kind
  return a.lookahead.index < b.lookahead.index

# --- New LALR(1) Data Structures (Kernel-Only with BitSet) ---

type
  CoreItem* = object
    ## The "core" of an LR item: just the rule position, without lookaheads.
    ## This is the key for LALR(1) state merging.
    variableIndex*: uint16         # Which non-terminal variable this production belongs to
    productionIndex*: uint16       # Which production of that variable
    position*: uint16              # Dot position (0 = before first symbol)
    inheritedPrecedence*: int16    # Precedence inherited from parent rule
  
  StateKernels* = stdtables.Table[CoreItem, LookaheadSet]
    ## Maps each core item to its set of lookaheads.
    ## This is the new state representation for LALR(1).

proc hash*(item: CoreItem): Hash =
  ## Efficient hash for CoreItem using combined field hashing.
  result = Hash(0)
  result = result !& hash(item.variableIndex)
  result = result !& hash(item.productionIndex)
  result = result !& hash(item.position)
  result = result !& hash(item.inheritedPrecedence)
  result = !$result

proc `==`*(a, b: CoreItem): bool =
  ## Equality for CoreItem.
  a.variableIndex == b.variableIndex and
  a.productionIndex == b.productionIndex and
  a.position == b.position and
  a.inheritedPrecedence == b.inheritedPrecedence

proc `<`*(a, b: CoreItem): bool =
  ## Ordering for CoreItem (for sorted sequences as map keys).
  if a.variableIndex != b.variableIndex: return a.variableIndex < b.variableIndex
  if a.productionIndex != b.productionIndex: return a.productionIndex < b.productionIndex
  if a.position != b.position: return a.position < b.position
  return a.inheritedPrecedence < b.inheritedPrecedence

# Toggle to use fast precomputed closure vs regular worklist closure
# Set to false to revert to safe, tested implementation
const USE_FAST_CLOSURE {.used.} = false  # TESTING: Debug precomputed closure; FAST_CLOSURE is actually slower ^-^

# Toggle to use new LALR(1) implementation with kernel-only storage and BitSet
# Set to true to enable the optimized LALR(1) algorithm
const USE_LALR_OPTIMIZED {.used.} = true  # Re-enabled after fixing cache bug

# Toggle to use Pager's LALR algorithm (propagation-based)
# Enabled for systematic debugging
const USE_PAGER_LALR {.used.} = true

proc precomputeClosureAdditions(grammar: SyntaxGrammar, firstSets: FirstSets): ClosurePrecomputation {.used.} =
  ## Precompute which items must be added when expanding each non-terminal.
  ## This is the tree-sitter optimization - compute once, use many times.
  result = ClosurePrecomputation(
    additions: newSeq[seq[TransitiveClosureAddition]](grammar.variables.len)
  )
  
  for i in 0 ..< grammar.variables.len:
    # For each non-terminal i, compute which non-terminals can appear
    # at the start of i's productions, along with their follow sets
    var followInfoByNonTerminal = stdtables.initTable[int, FollowSetInfo]()
    var stack = newSeq[(int, SymbolSet, bool)]()
    
    # Start with non-terminal i itself
    stack.add((i, initSymbolSet(), true))
    
    while stack.len > 0:
      let (symIdx, lookaheads, propagates) = stack.pop()
      
      # Get or create follow set info for this non-terminal
      if symIdx notin followInfoByNonTerminal:
        followInfoByNonTerminal[symIdx] = FollowSetInfo(
          lookaheads: initSymbolSet(),
          propagatesLookaheads: false
        )
      
      var info = followInfoByNonTerminal[symIdx]
      var didAdd = false
      
      # Merge lookaheads
      for la in lookaheads:
        if la notin info.lookaheads:
          info.lookaheads.incl(la)
          didAdd = true
      
      # Merge propagation flag
      if propagates and not info.propagatesLookaheads:
        info.propagatesLookaheads = true
        didAdd = true
      
      # If nothing changed, we've already processed this
      if not didAdd:
        continue
      
      # Store updated info
      followInfoByNonTerminal[symIdx] = info
      
      # Explore all productions of this non-terminal
      for production in grammar.variables[symIdx].productions:
        if production.steps.len > 0:
          let firstSym = production.steps[0].symbol
          
          if firstSym.kind == stNonTerminal:
            # First symbol is a non-terminal - need to explore it
            # Compute FIRST(β) where β = ALL symbols after firstSym
            var betaFirst = initSymbolSet()
            var allBetaNullable = true
            
            # FIX: Iterate through ALL symbols after the first, not just one
            for j in 1 ..< production.steps.len:
              let sym = production.steps[j].symbol
              
              if sym.kind == stNonTerminal:
                # Add FIRST(sym) to betaFirst
                if sym in firstSets:
                  for firstSym in firstSets[sym]:
                    betaFirst.incl(firstSym)
              else:
                # FIX: Handle external/terminal symbols directly
                betaFirst.incl(sym)
              
              # FIX: Check nullability to know when to stop
              if not isNullable(grammar, sym):
                allBetaNullable = false
                break
            
            # Compute final lookaheads for this expansion
            if allBetaNullable:
              # β is nullable (or empty) - propagate parent lookaheads AND add FIRST(β)
              var combined = betaFirst
              for la in lookaheads:
                combined.incl(la)
              stack.add((firstSym.index.int, combined, propagates))
            else:
              # β is not nullable - only use FIRST(β), don't propagate
              stack.add((firstSym.index.int, betaFirst, false))
    
    # Now store all additions for variable i
    for (varIdx, followInfo) in pairs(followInfoByNonTerminal):
      for prodIdx in 0 ..< grammar.variables[varIdx].productions.len:
        result.additions[i].add(TransitiveClosureAddition(
          variableIndex: varIdx,
          productionIndex: prodIdx,
          followInfo: followInfo
        ))

# === New LALR(1) Closure Precomputation (using BitSet) ===

proc precomputeClosureCache*(
    grammar: SyntaxGrammar,
    lexicalGrammar: LexicalGrammar,
    firstSets: FirstSets
): ClosureCache =
  ## Precompute closure expansions using BitSet for efficient lookahead operations.
  ## This is the optimized version for LALR(1) with kernel-only storage.
  
  # Create symbol context for bit mapping
  let terminalCount = lexicalGrammar.variables.len
  let externalCount = grammar.externalTokens.len
  let ctx = newSymbolContext(terminalCount, externalCount)
  
  result = ClosureCache(
    additions: newSeq[seq[ClosureAddition]](grammar.variables.len),
    symbolContext: ctx
  )
  
  # For each non-terminal, precompute what to add when we expand it
  for i in 0 ..< grammar.variables.len:
    var additionsByProd = stdtables.initTable[(uint16, uint16), LookaheadSetInfo]()
    var stack = newSeq[(int, LookaheadSet, bool)]()
    
    # Start with non-terminal i itself
    stack.add((i, initBitSet(ctx.maxIndex), true))
    
    while stack.len > 0:
      let (symIdx, lookaheads, propagates) = stack.pop()
      
      # For each production of this non-terminal
      for prodIdx in 0 ..< grammar.variables[symIdx].productions.len:
        let key = (symIdx.uint16, prodIdx.uint16)
        
        # Get or create lookahead info
        if key notin additionsByProd:
          additionsByProd[key] = LookaheadSetInfo(
            lookaheads: initBitSet(ctx.maxIndex),
            propagatesLookaheads: false
          )
        
        var info = additionsByProd[key]
        var changed = false
        
        # Merge lookaheads using BitSet union
        if info.lookaheads.union(lookaheads):
          changed = true
        
        # Merge propagation flag
        if propagates and not info.propagatesLookaheads:
          info.propagatesLookaheads = true
          changed = true
        
        # Store back
        additionsByProd[key] = info
        
        # If changed and production starts with non-terminal, explore it
        if changed:
          let production = grammar.variables[symIdx].productions[prodIdx]
          if production.steps.len > 0:
            let firstSym = production.steps[0].symbol
            
            if firstSym.kind == stNonTerminal:
              # Compute FIRST(β) where β = symbols after firstSym
              var betaFirst = initBitSet(ctx.maxIndex)
              var allBetaNullable = true
              
              for j in 1 ..< production.steps.len:
                let sym = production.steps[j].symbol
                
                if sym.kind == stNonTerminal:
                  # Add FIRST(sym) to betaFirst
                  if sym in firstSets:
                    for firstSym in firstSets[sym]:
                      let bit = ctx.symbolToBit(firstSym)
                      if bit >= 0:
                        betaFirst.incl(bit)
                else:
                  # Terminal or External
                  let bit = ctx.symbolToBit(sym)
                  if bit >= 0:
                    betaFirst.incl(bit)
                
                if not isNullable(grammar, sym):
                  allBetaNullable = false
                  break
              
              # Determine new lookaheads for firstSym expansion
              var newLookaheads = betaFirst
              let newPropagates = allBetaNullable and propagates
              
              # Add to stack for further exploration
              stack.add((firstSym.index.int, newLookaheads, newPropagates))
    
    # Convert table to seq - store ALL productions to add when expanding non-terminal i
    # Note: These might be productions of OTHER non-terminals that need to be added
    for key, followInfo in pairs(additionsByProd):
      let (varIdx, prodIdx) = key
      result.additions[i].add(ClosureAddition(
        variableIndex: varIdx,
        productionIndex: prodIdx,
        followInfo: followInfo
      ))

proc getTransitiveClosure*(
    grammar: SyntaxGrammar,
    kernels: StateKernels,
    cache: ClosureCache,
    firstSets: FirstSets
): StateKernels =
  ## Compute the transitive closure of a kernel set using the precomputed cache.
  ## Input: Kernel items (rule positions with their lookahead BitSets)
  ## Output: Full closure (kernels + all recursive expansions)
  ##
  ## This is the key LALR(1) optimization: we only store kernels in states,
  ## and compute closure on-the-fly when needed (during GOTO transitions).
  
  result = kernels  # Start with kernels
  var queue = newSeq[CoreItem]()
  for core in kernels.keys:
    queue.add(core)
  
  var visited = initHashSet[CoreItem]()
  
  while queue.len > 0:
    let item = queue.pop()
    if item in visited:
      continue
    visited.incl(item)
    
    # Look up what symbol is at the dot position
    let variable = grammar.variables[item.variableIndex]
    let production = variable.productions[item.productionIndex]
    
    # If dot is at end, nothing to expand
    if item.position >= production.steps.len.uint16:
      continue
    
    let symbol = production.steps[item.position].symbol
    
    # We only expand non-terminals
    if symbol.kind != stNonTerminal:
      continue
    
    # Look up precomputed expansions for this non-terminal
    let nonTermIdx = symbol.index.int
    if nonTermIdx >= cache.additions.len:
      continue
    
    # Get the item's current lookaheads
    if item notin result:
      continue  # Shouldn't happen
    let itemLookaheads = result[item]
    
    # === Compute Expansion Context ===
    # We are expanding A -> ... . B beta, L
    # The context for B's expansion is FIRST(beta L)
    # This equals FIRST(beta) U (if nullable(beta) then L else {})
    
    var expansionContext = initBitSet(cache.symbolContext.maxIndex)
    var betaNullable = true
    
    # Compute FIRST(beta)
    for k in (item.position + 1).int ..< production.steps.len:
      let betaSym = production.steps[k].symbol
      
      if betaSym.kind == stNonTerminal:
        if betaSym in firstSets:
          for firstSym in firstSets[betaSym]:
            let bit = cache.symbolContext.symbolToBit(firstSym)
            if bit >= 0:
              expansionContext.incl(bit)
      else:
        # Terminal/External
        let bit = cache.symbolContext.symbolToBit(betaSym)
        if bit >= 0:
          expansionContext.incl(bit)
      
      if not isNullable(grammar, betaSym):
        betaNullable = false
        break
    
    # If beta is nullable, include item's lookaheads (L)
    if betaNullable:
      discard expansionContext.union(itemLookaheads)
    
    # For each precomputed expansion
    for addition in cache.additions[nonTermIdx]:
      # Calculate precedence for the new item
      let stepPrec = production.steps[item.position].precedence
      let stepPrecVal = if stepPrec.kind == pkInteger: stepPrec.intValue.int16 else: 0'i16
      
      let nextInheritedPrec =
        if stepPrecVal != 0: stepPrecVal
        elif production.precedence != 0: production.precedence.int16
        else: item.inheritedPrecedence
      
      # Create the core for the new item
      let newCore = CoreItem(
        variableIndex: addition.variableIndex,
        productionIndex: addition.productionIndex,
        position: 0,  # Closure items always start at position 0
        inheritedPrecedence: nextInheritedPrec
      )
      
      # Calculate new lookaheads using the computed context
      var newLookaheads = addition.followInfo.lookaheads  # Static FIRST(content_suffix)
      
      # If the addition propagates lookaheads, merge with our expansion context
      if addition.followInfo.propagatesLookaheads:
        discard newLookaheads.union(expansionContext)
      
      # Add to result or union with existing
      var changed = false
      if newCore in result:
        # Merge lookaheads with existing core
        changed = result[newCore].union(newLookaheads)
      else:
        # New core - add it
        result[newCore] = newLookaheads
        changed = true
      
      # If lookaheads changed, add to queue for further expansion
      if changed and newCore notin visited:
        queue.add(newCore)

type
  FirstBetaCacheKey* = object
    variableIndex*: uint16
    productionIndex*: uint16
    position*: uint16

proc hash*(cacheKey: FirstBetaCacheKey): Hash {.inline.} =
  when nimvm:
    for fld, val in cacheKey.fieldPairs:
      result = result !& hash(val)
  else:
    result = hash(cast[int](cacheKey))

proc closure*(
  grammar: SyntaxGrammar,
  items: HashSet[LR1Item],
  firstSets: FirstSets): HashSet[LR1Item] =
  ## Compute closure using a Worklist Algorithm (O(N) instead of O(N^2))
  result = items
  
  # Worklist: only process items that haven't been expanded yet
  # Pre-allocate with reasonable capacity to reduce reallocations
  var stack = newSeqOfCap[LR1Item](items.len * 4)  # Heuristic: avg 4x expansion
  for item in items: stack.add(item)
  
  # Phase 2: FIRST(β) memoization cache
  # Key: (variableIndex, productionIndex, position) → (FIRST set, isNullable)
  var firstBetaCache = stdtables.initTable[FirstBetaCacheKey, (SymbolSet, bool)]()
  
  while stack.len > 0:
    let item = stack.pop()
    
    let variable = grammar.variables[item.variableIndex]
    let production = variable.productions[item.productionIndex]
    
    # If dot is at the end, nothing to expand
    if item.position >= production.steps.len:
      continue
    
    let nextSym = production.steps[item.position].symbol
    
    # We only expand Non-Terminals
    if nextSym.kind != stNonTerminal:
      continue
      
    # --- Lookahead Computation with Memoization ---
    # We need FIRST(beta + item.lookahead), where beta is symbols after nextSym
    
    # Check cache for FIRST(β) where β = symbols after position
    let cacheKey = FirstBetaCacheKey(
      variableIndex: item.variableIndex.uint16,
      productionIndex: item.productionIndex.uint16,
      position: item.position.uint16
    )
    var lookaheads: SymbolSet
    var allBetaNullable: bool
    
    if cacheKey in firstBetaCache:
      # Cache hit - reuse computed FIRST(β)
      let (cachedFirst, cachedNullable) = firstBetaCache[cacheKey]
      lookaheads = cachedFirst  # Copy the set
      allBetaNullable = cachedNullable
    else:
      # Cache miss - compute FIRST(β)
      lookaheads = initSymbolSet(8)  # Typical lookahead set size
      allBetaNullable = true
      
      # Check symbols after the current non-terminal (beta)
      for i in (item.position + 1) ..< production.steps.len:
        let sym = production.steps[i].symbol
        
        if sym.kind == stNonTerminal:
          if sym in firstSets:
            for firstSym in firstSets[sym]:
              lookaheads.incl(firstSym)
        else:
          # Terminal/External
          lookaheads.incl(sym)
        
        if not isNullable(grammar, sym):
          allBetaNullable = false
          break
      
      # Store in cache for future use
      firstBetaCache[cacheKey] = (lookaheads, allBetaNullable)
    
    # If everything after nextSym is nullable, we inherit the item's lookahead
    if allBetaNullable:
      lookaheads.incl(item.lookahead)
      
    # --- Precedence Computation ---
    let stepPrec = production.steps[item.position].precedence
    let stepPrecVal = if stepPrec.kind == pkInteger: stepPrec.intValue else: 0
    
    let nextInheritedPrec = 
      if stepPrecVal != 0: stepPrecVal
      elif production.precedence != 0: production.precedence 
      else: item.inheritedPrecedence

    # --- Add New Items ---
    let nextVarIndex = nextSym.index
    # Iterate all productions of the non-terminal B
    for prodIdx in 0 ..< grammar.variables[nextVarIndex].productions.len:
      for la in lookaheads:
        let newItem = LR1Item(
          variableIndex: nextVarIndex.int32,
          productionIndex: prodIdx.int32,
          position: 0, # Closure items always start at 0
          lookahead: la,
          inheritedPrecedence: nextInheritedPrec
        )
        
        # KEY OPTIMIZATION:
        # If we haven't seen this exact item before, add it to result AND stack.
        # If we HAVE seen it, we don't need to expand it again.
        if newItem notin result:
          result.incl(newItem)
          stack.add(newItem)

proc closureFast*(
  grammar: SyntaxGrammar,
  items: HashSet[LR1Item],
  firstSets: FirstSets,
  precomputed: ClosurePrecomputation
): HashSet[LR1Item] =
  result = initHashSet[LR1Item](items.len * 4)
  var stack = newSeq[LR1Item]()
  
  # Initialize with kernel items
  for item in items:
    result.incl(item)
    stack.add(item)
  
  # Process worklist - expand each item
  while stack.len > 0:
    let item = stack.pop()
    
    let variable = grammar.variables[item.variableIndex]
    let production = variable.productions[item.productionIndex]
    
    # If dot is at the end, nothing to expand
    if item.position >= production.steps.len:
      continue
    
    let nextSym = production.steps[item.position].symbol
    
    # Only expand non-terminals
    if nextSym.kind != stNonTerminal:
      continue
    
    # Compute FIRST(β) for ALL symbols after current position
    var followingTokens = initSymbolSet(16)
    var allNullable = true
    
    for i in (item.position + 1) ..< production.steps.len:
      let sym = production.steps[i].symbol
      
      if sym.kind == stNonTerminal:
        if sym in firstSets:
          for f in firstSets[sym]:
            followingTokens.incl(f)
      else:
        # Handle external/terminal symbols
        followingTokens.incl(sym)
      
      if not isNullable(grammar, sym):
        allNullable = false
        break
    
    # If all symbols after are nullable, add item's lookahead
    if allNullable:
      followingTokens.incl(item.lookahead)
    
    # Get precedence
    let stepPrec = production.steps[item.position].precedence
    let stepPrecVal = if stepPrec.kind == pkInteger: stepPrec.intValue else: 0
    let nextInheritedPrec = 
      if stepPrecVal != 0: stepPrecVal
      elif production.precedence != 0: production.precedence
      else: item.inheritedPrecedence
    
    # Use precomputed additions with corrected lookaheads
    for addition in precomputed.additions[nextSym.index]:
      var lookaheads = initSymbolSet(32)
      
      # Add fixed lookaheads from precomputation
      for la in addition.followInfo.lookaheads:
        lookaheads.incl(la)
      
      # Add propagated lookaheads if applicable
      if addition.followInfo.propagatesLookaheads:
        for la in followingTokens:
          lookaheads.incl(la)
      
      # Create items for each lookahead
      for la in lookaheads:
        let newItem = LR1Item(
          variableIndex: addition.variableIndex.int32,
          productionIndex: addition.productionIndex.int32,
          position: 0,
          lookahead: la,
          inheritedPrecedence: nextInheritedPrec
        )
        
        # Add to result AND stack if new
        if newItem notin result:
          result.incl(newItem)
          stack.add(newItem)  # ← FIX: Must expand this item too!

proc goto*(grammar: SyntaxGrammar, items: HashSet[LR1Item], symbol: GrammarSymbol, firstSets: FirstSets): HashSet[LR1Item] =
  ## Compute GOTO(items, symbol)
  var moved = initHashSet[LR1Item](items.len)  # Pre-size based on input
  
  for item in items:
    let variable = grammar.variables[item.variableIndex]
    let production = variable.productions[item.productionIndex]
    
    # Check if dot is before the symbol
    if item.position >= production.steps.len:
      continue
    
    let nextSym = production.steps[item.position].symbol
    if nextSym == symbol:
      # Move dot over symbol
      moved.incl(LR1Item(
        variableIndex: item.variableIndex,
        productionIndex: item.productionIndex,
        position: item.position + 1,
        lookahead: item.lookahead,
        inheritedPrecedence: item.inheritedPrecedence
      ))
  
  # Return closure of moved items
  if moved.len > 0:
    closure(grammar, moved, firstSets)
  else:
    initHashSet[LR1Item]()

proc mergeStates(
    states: var seq[HashSet[LR1Item]], 
    transitions: var seq[stdtables.Table[GrammarSymbol, int]]
) =
  ## Merge compatible LR(1) states (LALR optimization)
  ## States are compatible if they have the same core items (ignoring lookaheads)
  echo "[Treestand] Merging compatible states (LALR optimization)..."

  
  var coreToState = stdtables.initTable[seq[LR1Item], int]()
  var stateRemap = stdtables.initTable[int, int]() # Old ID -> New ID
  
  var mergedStates = newSeq[HashSet[LR1Item]]()
  var nextStateId = 0
  
  for i in 0 ..< states.len:
    # Build core key (ignore lookahead)
    var core = newSeq[LR1Item]()
    for item in states[i]:
      var coreItem = item
      # Use a unified dummy lookahead for core comparison
      coreItem.lookahead = GrammarSymbol(kind: stEnd, index: 0)
      core.add(coreItem)
    # Sort to ensure canonical key
    core.sort()
    
    if core in coreToState:
      # Found existing state with same core -> Merge!
      let targetId = coreToState[core]
      stateRemap[i] = targetId
      
      # Merge lookaheads into the existing state
      for item in states[i]:
        # HashSet automatically handles the union logic
        # Since items differ only by lookahead, adding them unions the lookaheads
        mergedStates[targetId].incl(item)
    else:
      # New unique core found
      let targetId = nextStateId
      coreToState[core] = targetId
      stateRemap[i] = targetId
      
      # Add new state (copy initial items)
      mergedStates.add(states[i])
      inc(nextStateId)
      
  echo "[Treestand] Reduced states from ", states.len, " to ", mergedStates.len
  
  # Update transitions for the new merged states
  var newTransitions = newSeq[stdtables.Table[GrammarSymbol, int]](mergedStates.len)
  for i in 0 ..< newTransitions.len:
    newTransitions[i] = stdtables.initTable[GrammarSymbol, int]()
    
  for oldStateId, transTable in transitions:
    # If this old state was merged into a target state
    if oldStateId in stateRemap:
        let newStateId = stateRemap[oldStateId]
        
        for sym, oldTargetId in transTable:
          let newTargetId = stateRemap[oldTargetId]
          
          if sym in newTransitions[newStateId]:
            # Consistent transition check (should match if cores are same)
            if newTransitions[newStateId][sym] != newTargetId:
               # In standard LALR, this shouldn't happen for valid cores
               # But if it does, it's a conflict
               discard 
          else:
            newTransitions[newStateId][sym] = newTargetId
            
  # Replace with merged data
  states = mergedStates
  transitions = newTransitions

proc buildCanonicalCollection*(grammar: SyntaxGrammar, firstSets: FirstSets, augmentedStartIndex: int = -1): tuple[states: seq[HashSet[LR1Item]], stateMap: stdtables.Table[seq[LR1Item], int], transitions: seq[stdtables.Table[GrammarSymbol, int]]] =
  # LALR(1) construction: merge states by core during construction
  # This matches tree-sitter's algorithm for better state compression
  const estimatedStates = 2048
  var states = newSeqOfCap[HashSet[LR1Item]](estimatedStates)
  var stateMap = stdtables.initTable[seq[LR1Item], int](estimatedStates)
  var transitions = newSeqOfCap[stdtables.Table[GrammarSymbol, int]](estimatedStates)
  var workList = initDeque[int]()
  let startVarIndex = if augmentedStartIndex >= 0: augmentedStartIndex else: 0
 
  # Precompute closure additions if using fast closure
  when USE_FAST_CLOSURE:
    echo "[Treestand] Precomputing closure additions for fast closure..."
    let closurePrecomp = precomputeClosureAdditions(grammar, firstSets)
    echo "[Treestand] Precomputation complete. Using closureFast."
  
  let initialItem = LR1Item(
    variableIndex: startVarIndex.int32,
    productionIndex: 0.int32,
    position: 0.int32,
    lookahead: GrammarSymbol(kind: stEnd, index: 0),
    inheritedPrecedence: 0
  )
  
  let initialKernel = @[initialItem]
  var initialSet = initHashSet[LR1Item]()
  initialSet.incl(initialItem)
  
  # DEBUGGING: Compare both implementations
  when USE_FAST_CLOSURE:
    let fastClosure = closureFast(grammar, initialSet, firstSets, closurePrecomp)
    let slowClosure = closure(grammar, initialSet, firstSets)
    
    if fastClosure.len != slowClosure.len:
      echo "[DEBUG] Initial closure DIVERGENCE!"
      echo "  Fast: ", fastClosure.len, " items"
      echo "  Slow: ", slowClosure.len, " items"
      
      for item in slowClosure:
        if item notin fastClosure:
          let v = grammar.variables[item.variableIndex]
          let p = v.productions[item.productionIndex]
          echo "  MISSING in fast: ", v.name, " prod=", item.productionIndex, " pos=", item.position
      
      for item in fastClosure:
        if item notin slowClosure:
          let v = grammar.variables[item.variableIndex]
          echo "  EXTRA in fast: ", v.name, " prod=", item.productionIndex, " pos=", item.position
    
    let initialClosure = fastClosure
  else:
    let initialClosure = closure(grammar, initialSet, firstSets)
  
  states.add(initialClosure)
  stateMap[initialKernel] = 0
  transitions.add(stdtables.initTable[GrammarSymbol, int]())
  workList.addLast(0)
  
  echo "[Treestand] Starting canonical collection construction."
  var steps = 0
  
  while workList.len > 0:
    let stateId = workList.popFirst()
    let currentSet = states[stateId]
    
    inc steps
    if steps mod 500 == 0:
      debugEchoMsg "[BuildTables] Processing state ", stateId, ". Total states: ", states.len
    
    # Map: Symbol → List of Kernel Items for the next state
    var nextStateKernels = stdtables.initTable[GrammarSymbol, seq[LR1Item]]()
    
    for item in currentSet:
      let variable = grammar.variables[item.variableIndex]
      let production = variable.productions[item.productionIndex]
      
      # If dot is not at end, we can transition
      if item.position < production.steps.len:
        let symbol = production.steps[item.position].symbol
        
        # Create the item as it would appear in the next state (dot moved)
        let movedItem = LR1Item(
           variableIndex: item.variableIndex,
           productionIndex: item.productionIndex,
           position: item.position + 1,
           lookahead: item.lookahead,
           inheritedPrecedence: item.inheritedPrecedence
        )
        
        if symbol notin nextStateKernels:
          nextStateKernels[symbol] = newSeq[LR1Item]()
        
        nextStateKernels[symbol].add(movedItem)

    # Now process the groups
    for symbol, unsortedKernel in nextStateKernels:
      var nextKernel = unsortedKernel
      nextKernel.sort() # Sort for map key
      
      var targetStateId: int
      
      # Natural LALR: if this exact kernel already exists, reuse it
      if nextKernel in stateMap:
        targetStateId = stateMap[nextKernel]
      else:
        # Create new state
        var kernelSet = initHashSet[LR1Item]()
        for item in nextKernel: kernelSet.incl(item)
        
        # Use fast or regular closure based on toggle
        when USE_FAST_CLOSURE:
          let fullState = closureFast(grammar, kernelSet, firstSets, closurePrecomp)
        else:
          let fullState = closure(grammar, kernelSet, firstSets)
        
        targetStateId = states.len
        states.add(fullState)
        stateMap[nextKernel] = targetStateId
        transitions.add(stdtables.initTable[GrammarSymbol, int]())
        workList.addLast(targetStateId)
      
      transitions[stateId][symbol] = targetStateId

  echo "[Treestand] Canonical collection complete. Total states before merge: ", states.len
  
  # LALR Merging: combine states with same core but different lookaheads
  mergeStates(states, transitions)
  echo "[Treestand] Total states after LALR merge: ", states.len
  
  (states, stateMap, transitions)

# === New LALR(1) State Construction with Kernel-Only Storage ===

proc buildCanonicalCollectionLALR*(
    grammar: SyntaxGrammar,
    lexicalGrammar: LexicalGrammar,
    firstSets: FirstSets,
    augmentedStartIndex: int = -1
): tuple[states: seq[StateKernels], stateMap: stdtables.Table[seq[CoreItem], seq[int]], transitions: seq[stdtables.Table[GrammarSymbol, int]]] =
  ## Build LALR(1) state collection using kernel-only storage with BitSet lookaheads.
  ## This is the optimized implementation that:
  ## 1. Stores only kernels in states (compute closure on-demand)
  ## 2. Uses BitSet for O(1) lookahead operations
  ## 3. Merges states during construction when kernels match
  ## 4. Propagates lookahead changes back through the worklist
  
  echo "[Treestand] Building LALR(1) canonical collection with kernel-only storage..."
  let startTime = cpuTime()
  
  # Precompute closure cache
  echo "[Treestand] Precomputing closure cache..."
  let closureCache = precomputeClosureCache(grammar, lexicalGrammar, firstSets)
  echo "[Treestand] Closure cache complete."
  
  const estimatedStates = 512  # LALR needs far fewer states than Canonical LR(1)
  var states = newSeqOfCap[StateKernels](estimatedStates)
  var stateMap = stdtables.initTable[seq[CoreItem], seq[int]](estimatedStates)
  var transitions = newSeqOfCap[stdtables.Table[GrammarSymbol, int]](estimatedStates)
  var workList = initDeque[int]()
  
  let startVarIndex = if augmentedStartIndex >= 0: augmentedStartIndex else: 0
  
  # Create initial kernel item
  let initialCore = CoreItem(
    variableIndex: startVarIndex.uint16,
    productionIndex: 0,
    position: 0,
    inheritedPrecedence: 0
  )
  
  # Initial lookahead is EOF (end symbol)
  var initialLookaheads = initBitSet(closureCache.symbolContext.maxIndex)
  let eofBit = closureCache.symbolContext.symbolToBit(GrammarSymbol(kind: stEnd, index: 0))
  initialLookaheads.incl(eofBit)
  
  # Create initial kernel set
  var initialKernels = stdtables.initTable[CoreItem, LookaheadSet]()
  initialKernels[initialCore] = initialLookaheads
  
  # Add initial state
  states.add(initialKernels)
  stateMap[@[initialCore]] = @[0]
  transitions.add(stdtables.initTable[GrammarSymbol, int]())
  workList.addLast(0)
  
  echo "[Treestand] Starting LALR construction..."
  var steps = 0
  
  while workList.len > 0:
    let stateId = workList.popFirst()
    let kernels = states[stateId]
    
    inc steps
    if steps mod 100 == 0:
      debugEchoMsg "[BuildTables] Processing state ", stateId, ". Total states: ", states.len
    
    # Compute closure on-demand
    let fullClosure = getTransitiveClosure(grammar, kernels, closureCache, firstSets)
    
    # Group items by next symbol (GOTO operation)
    var nextStateKernels = stdtables.initTable[GrammarSymbol, StateKernels]()
    
    for core, lookaheads in fullClosure:
      let variable = grammar.variables[core.variableIndex]
      let production = variable.productions[core.productionIndex]
      
      # Skip if dot is at end
      if core.position >= production.steps.len.uint16:
        continue
      
      let symbol = production.steps[core.position].symbol
      
      # Create moved item (dot advanced)
      let movedCore = CoreItem(
        variableIndex: core.variableIndex,
        productionIndex: core.productionIndex,
        position: core.position + 1,
        inheritedPrecedence: core.inheritedPrecedence
      )
      
      # Initialize group for this symbol if needed
      if symbol notin nextStateKernels:
        nextStateKernels[symbol] = stdtables.initTable[CoreItem, LookaheadSet]()
      
      # Add/merge lookaheads
      if movedCore in nextStateKernels[symbol]:
        discard nextStateKernels[symbol][movedCore].union(lookaheads)
      else:
        nextStateKernels[symbol][movedCore] = lookaheads
    
    # Process each symbol transition
    for symbol, nextKernels in nextStateKernels:
      # Create canonical key from kernel cores (sorted for consistency)
      var kernelCores = newSeq[CoreItem]()
      for core in nextKernels.keys:
        kernelCores.add(core)
      kernelCores.sort()
      
      var targetStateId: int = -1
      
      # LALR merging: check if this kernel (core only) already exists
      if kernelCores in stateMap:
        # Check candidates for compatibility
        # Compatibility condition: Merging must not change the set of VALID EXTERNAL TOKENS
        
        # debugEchoMsg "[LALR] Kernel exists, checking ", stateMap[kernelCores].len, " candidates for merge"
        for candidateId in stateMap[kernelCores]:
           # Optimization: Just check if Externals(Candidate) == Externals(Next)
           var externals1 = initBitSet(closureCache.symbolContext.maxIndex)
           var externals2 = initBitSet(closureCache.symbolContext.maxIndex)
           
           for _, la in states[candidateId]:
             for bit in la:
                let s = closureCache.symbolContext.bitToSymbol(bit)
                if s.kind == stExternal: externals1.incl(bit)
           
           for _, la in nextKernels:
             for bit in la:
                let s = closureCache.symbolContext.bitToSymbol(bit)
                if s.kind == stExternal: externals2.incl(bit)
           
           if externals1 == externals2:
            # debugEchoMsg "[LALR] External tokens match, merging into state ", candidateId
             targetStateId = candidateId
             break
           else:
            discard
             # debugEchoMsg "[LALR] External tokens differ, trying next candidate"
        
        
        if targetStateId == -1:
          debugEchoMsg "[LALR] No compatible candidate found, creating new state"
        
      if targetStateId != -1:
        # Kernel exists and is compatible - merge lookaheads
        var changed = false
        
        for core, lookaheads in nextKernels:
          if core in states[targetStateId]:
            # Union lookaheads
            if states[targetStateId][core].union(lookaheads):
              changed = true
          else:
            # New core in existing state (shouldn't happen if kernel keys are correct)
            states[targetStateId][core] = lookaheads
            changed = true
        
        # Propagation: if lookaheads changed, re-process this state
        if changed and targetStateId notin workList:
          workList.addLast(targetStateId)
      else:
        # Create new state
        targetStateId = states.len
        states.add(nextKernels)
        # Add to list of states for this core
        if kernelCores notin stateMap:
           stateMap[kernelCores] = @[targetStateId]
        else:
           debugEchoMsg "[LALR] Adding state ", targetStateId, " to existing kernel (now ", stateMap[kernelCores].len + 1, " states for this kernel)"
           stateMap[kernelCores].add(targetStateId)
           
        transitions.add(stdtables.initTable[GrammarSymbol, int]())
        workList.addLast(targetStateId)
      
      # Record transition
      transitions[stateId][symbol] = targetStateId
  
  let elapsed = cpuTime() - startTime
  echo "[Treestand] LALR(1) collection complete. Total states: ", states.len, " (built in ", elapsed.formatFloat(ffDecimal, 3), "s)"
  
  (states, stateMap, transitions)

# Converter: StateKernels -> HashSet[LR1Item] for integration with existing code
proc convertStateKernelsToLR1Items(
    kernels: StateKernels,
    ctx: SymbolContext
): HashSet[LR1Item] =
  ## Convert StateKernels (BitSet lookaheads) to HashSet[LR1Item] for compatibility.
  result = initHashSet[LR1Item]()
  
  for core, lookaheads in kernels:
    # Expand each core with all its lookaheads
    for bitIndex in lookaheads:
      let lookaheadSym = ctx.bitToSymbol(bitIndex)
      let item = LR1Item(
        variableIndex: core.variableIndex.int32,
        productionIndex: core.productionIndex.int32,
        position: core.position.int32,
        lookahead: lookaheadSym,
        inheritedPrecedence: core.inheritedPrecedence.int32
      )
      result.incl(item)

# ==================================================================
# Pager's LALR Algorithm Support
# ==================================================================
# Hash and equality for LR0Item (needed for hash tables/sets)
proc hash(item: LR0Item): Hash =
  var h: Hash = 0
  h = h !& hash(item.variableIndex)
  h = h !& hash(item.productionIndex)
  h = h !& hash(item.position)
  h = h !& hash(item.inheritedPrecedence)
  result = !$h

proc `==`(a, b: LR0Item): bool =
  a.variableIndex == b.variableIndex and
  a.productionIndex == b.productionIndex and
  a.position == b.position and
  a.inheritedPrecedence == b.inheritedPrecedence

proc `<`*(a, b: LR0Item): bool =
  ## Ordering for sorting LR0Items
  if a.variableIndex != b.variableIndex:
    return a.variableIndex < b.variableIndex
  if a.productionIndex != b.productionIndex:
    return a.productionIndex < b.productionIndex
  if a.position != b.position:
    return a.position < b.position
  return a.inheritedPrecedence < b.inheritedPrecedence

# LR(0) Closure - compute closure without lookaheads
proc computeLR0Closure(
  kernels: seq[LR0Item],
  grammar: SyntaxGrammar
): HashSet[LR0Item] =
  ## Compute LR(0) closure: expand items until fixed point
  # debugEchoMsg "[Pager] Computing LR(0) closure for ", kernels.len, " kernels"
  
  result = initHashSet[LR0Item]()
  var workList = initDeque[LR0Item]()
  
  # Add all kernels
  for item in kernels:
    result.incl(item)
    workList.addLast(item)
  
  while workList.len > 0:
    let item = workList.popFirst()
    let variable = grammar.variables[item.variableIndex]
    let production = variable.productions[item.productionIndex]
    
    # Check if dot is before a non-terminal
    if item.position < production.steps.len.uint16:
      let nextSymbol = production.steps[item.position].symbol
      
      if nextSymbol.kind == stNonTerminal:
        # Add all productions of this non-terminal
        let nextVar = grammar.variables[nextSymbol.index]
        for prodIdx in 0 ..< nextVar.productions.len:
          let prec = production.steps[item.position].precedence
          let precedenceVal = if prec.kind == pkInteger: prec.intValue else: 0
          
          let newItem = LR0Item(
            variableIndex: nextSymbol.index.uint16,
            productionIndex: prodIdx.uint16,
            position: 0,
            inheritedPrecedence: precedenceVal
          )
          
          if newItem notin result:
            result.incl(newItem)
            workList.addLast(newItem)

# Build LR(0) automaton
proc buildLR0Automaton(
  grammar: SyntaxGrammar,
  augmentedStartIndex: int
): (seq[seq[LR0Item]], seq[stdtables.Table[GrammarSymbol, int]]) =
  ## Build LR(0) automaton: states contain only kernel items (no lookaheads)
  debugEchoMsg "[Pager] Building LR(0) automaton..."
  
  var states = newSeq[seq[LR0Item]]()
  var stateMap = stdtables.initTable[seq[LR0Item], int]()
  var transitions = newSeq[stdtables.Table[GrammarSymbol, int]]()
  var workList = initDeque[int]()
  
  # Initial state: S' -> •S
  let initialItem = LR0Item(
    variableIndex: augmentedStartIndex.uint16,
    productionIndex: 0,
    position: 0,
    inheritedPrecedence: 0
  )
  
  var initialKernel = @[initialItem]
  states.add(initialKernel)
  stateMap[initialKernel] = 0
  transitions.add(stdtables.initTable[GrammarSymbol, int]())
  workList.addLast(0)
  
  var stepsProcessed = 0
  while workList.len > 0:
    let stateId = workList.popFirst()
    let kernels = states[stateId]
    
    inc stepsProcessed
    if stepsProcessed mod 100 == 0:
      debugEchoMsg "[Pager] Processed ", stepsProcessed, " states, total: ", states.len
    
    # Compute closure
    let closure = computeLR0Closure(kernels, grammar)
    
    # Group by next symbol (GOTO)
    var gotoSets = stdtables.initTable[GrammarSymbol, seq[LR0Item]]()
    
    for item in closure:
      let variable = grammar.variables[item.variableIndex]
      let production = variable.productions[item.productionIndex]
      
      # Can we shift?
      if item.position < production.steps.len.uint16:
        let symbol = production.steps[item.position].symbol
        
        # Move dot over symbol
        let movedItem = LR0Item(
          variableIndex: item.variableIndex,
          productionIndex: item.productionIndex,
          position: item.position + 1,
          inheritedPrecedence: item.inheritedPrecedence
        )
        
        if symbol notin gotoSets:
          gotoSets[symbol] = @[]
        gotoSets[symbol].add(movedItem)
    
    # Create/find target states
    for symbol, unsortedKernel in gotoSets:
      var kernel = unsortedKernel
      kernel.sort()
      
      var targetStateId: int
      if kernel in stateMap:
        targetStateId = stateMap[kernel]
      else:
        targetStateId = states.len
        states.add(kernel)
        stateMap[kernel] = targetStateId
        transitions.add(stdtables.initTable[GrammarSymbol, int]())
        workList.addLast(targetStateId)
      
      transitions[stateId][symbol] = targetStateId
  
  debugEchoMsg "[Pager] LR(0) automaton complete: ", states.len, " states"
  (states, transitions)

# Compute lookahead propagations using Pager's algorithm
proc computeLookaheadPropagations(
  lr0States: seq[seq[LR0Item]],
  transitions: seq[stdtables.Table[GrammarSymbol, int]],
  grammar: SyntaxGrammar,
  lexicalGrammar: LexicalGrammar,
  firstSets: FirstSets,
  ctx: SymbolContext
): seq[LR0State] =
  ## Compute spontaneous lookaheads and propagation links for each LR(0) state
  ## KEY FIX: Process ALL closure items, not just kernels!
  debugEchoMsg "[Pager] Computing lookahead propagations..."
  
  result = newSeq[LR0State](lr0States.len)
  
  # Initialize states with empty tables
  for i in 0 ..< lr0States.len:
    result[i] = LR0State(
      kernels: lr0States[i],
      lookaheads: stdtables.initTable[LR0Item, LookaheadSet](),
      spontaneous: stdtables.initTable[LR0Item, LookaheadSet](),
      propagations: stdtables.initTable[LR0Item, seq[PropagationLink]]()
    )
    
    # Initialize lookahead sets for all kernels
    for kernel in lr0States[i]:
      result[i].lookaheads[kernel] = initBitSet(ctx.maxIndex)
      result[i].spontaneous[kernel] = initBitSet(ctx.maxIndex)
      result[i].propagations[kernel] = @[]
  
  # Special: Add EOF to initial state's initial item
  if lr0States.len > 0 and lr0States[0].len > 0:
    let eofBit = ctx.symbolToBit(GrammarSymbol(kind: stEnd, index: 0))
    result[0].spontaneous[lr0States[0][0]].incl(eofBit)
    result[0].lookaheads[lr0States[0][0]].incl(eofBit)
  
  debugEchoMsg "[Pager] Processing ", lr0States.len, " states for propagation..."
  var statesProcessed = 0
  
  for stateId in 0 ..< lr0States.len:
    inc statesProcessed
    if statesProcessed mod 100 == 0:
      debugEchoMsg "[Pager] Propagation: processed ", statesProcessed, " of ", lr0States.len
    
    # Process each kernel item specifically to trace dummy lookahead flow
    for kernel in lr0States[stateId]:
      
      # Queue for closure: (item, propagates, spontaneous)
      # propagates: true if this item carries the dummy lookahead from the kernel
      # spontaneous: lookaheads generated between the kernel and this item
      var closureQueue = initDeque[tuple[item: LR0Item, propagates: bool, spontaneous: SymbolSet]]()
      var closureInfo = stdtables.initTable[LR0Item, tuple[propagates: bool, spontaneous: SymbolSet]]()
      
      # Init with kernel
      let startInfo = (propagates: true, spontaneous: initSymbolSet())
      closureQueue.addLast((item: kernel, propagates: true, spontaneous: initSymbolSet()))
      closureInfo[kernel] = startInfo
      
      while closureQueue.len > 0:
        let (currentItem, currentProp, currentSpont) = closureQueue.popFirst()
        
        let variable = grammar.variables[currentItem.variableIndex]
        let production = variable.productions[currentItem.productionIndex]
        
        # 1. Expand (Closure)
        if currentItem.position < production.steps.len.uint16:
          let step = production.steps[currentItem.position]
          if step.symbol.kind == stNonTerminal:
            let targetVarIdx = step.symbol.index
            let targetVar = grammar.variables[targetVarIdx]
            
            # Calculate FIRST(beta)
            let betaStart = currentItem.position + 1
            var betaFirst = initSymbolSet()
            var betaNullable = true
            
            if betaStart < production.steps.len.uint16:
              let betaSteps = production.steps[betaStart.int .. ^1]
              for bStep in betaSteps:
                # Terminals provide their own lookahead, variables use firstSets
                if bStep.symbol.kind == stNonTerminal:
                  let f = firstSets.getOrDefault(bStep.symbol, initSymbolSet())
                  for s in f:
                     betaFirst.incl(s)
                  
                  # Check nullability
                  if grammar.variables[bStep.symbol.index].isNullable:
                     discard
                  else:
                     betaNullable = false
                     break
                else:
                  # Terminal or other symbol
                  betaFirst.incl(bStep.symbol)
                  betaNullable = false
                  break
            
            # Combine logic
            let nextPropagates = currentProp and betaNullable
            var nextSpont = betaFirst
            if betaNullable:
               for s in currentSpont: nextSpont.incl(s)
            
            # Add all productions of targetVar
            for prodIdx, _ in targetVar.productions:
              let prec = step.precedence
              let precedenceVal = if prec.kind == pkInteger: prec.intValue else: 0
              let newItem = LR0Item(
                variableIndex: targetVarIdx.uint16,
                productionIndex: prodIdx.uint16,
                position: 0,
                inheritedPrecedence: int16(precedenceVal)
              )
              
              # Merge with existing info if any
              var changed = false
              if newItem in closureInfo:
                var info = closureInfo[newItem]
                if not info.propagates and nextPropagates:
                  info.propagates = true
                  changed = true
                for s in nextSpont:
                  if not info.spontaneous.contains(s):
                    info.spontaneous.incl(s)
                    changed = true
                closureInfo[newItem] = info
              else:
                closureInfo[newItem] = (propagates: nextPropagates, spontaneous: nextSpont)
                changed = true
              
              if changed:
                closureQueue.addLast((item: newItem, propagates: closureInfo[newItem].propagates, spontaneous: closureInfo[newItem].spontaneous))
        
        # 2. Shift (Transition)
        if currentItem.position < production.steps.len.uint16:
          let symbol = production.steps[currentItem.position].symbol
          if symbol in transitions[stateId]:
             let targetStateId = transitions[stateId][symbol]
             let movedItem = LR0Item(
               variableIndex: currentItem.variableIndex,
               productionIndex: currentItem.productionIndex,
               position: currentItem.position + 1,
               inheritedPrecedence: currentItem.inheritedPrecedence
             )
             
             # Create propagation link if propagates
             if currentProp:
               # Ensure target lists exist
               if movedItem notin result[targetStateId].propagations:
                  result[targetStateId].propagations[movedItem] = @[]
               
               # Add link
               var exists = false
               for link in result[stateId].propagations[kernel]:
                 if link.targetStateId == targetStateId and link.targetItem == movedItem:
                   exists = true; break
               if not exists:
                 result[stateId].propagations[kernel].add(PropagationLink(
                   targetStateId: targetStateId,
                   targetItem: movedItem
                 ))
             
             # Add spontaneous lookaheads
             for s in currentSpont:
                 let bit = ctx.symbolToBit(s)
                 if movedItem notin result[targetStateId].spontaneous:
                    result[targetStateId].spontaneous[movedItem] = initBitSet(ctx.maxIndex)
                 if movedItem notin result[targetStateId].lookaheads:
                    result[targetStateId].lookaheads[movedItem] = initBitSet(ctx.maxIndex)
                    
                 result[targetStateId].spontaneous[movedItem].incl(bit)
                 result[targetStateId].lookaheads[movedItem].incl(bit)
  
  debugEchoMsg "[Pager] Propagation computation complete"

# Propagate lookaheads iteratively until fixed point
proc propagateLookaheads(
  states: var seq[LR0State]
): void =
  ## Iteratively propagate lookaheads through the graph until no changes
  debugEchoMsg "[Pager] Propagating lookaheads..."
  
  var changed = true
  var iterations = 0
  
  while changed:
    changed = false
    inc iterations
    
    if iterations mod 10 == 0:
      debugEchoMsg "[Pager] Propagation iteration ", iterations
    
    for stateId in 0 ..< states.len:
      for kernel, propagations in states[stateId].propagations:
        if kernel notin states[stateId].lookaheads:
          continue
        
        let sourceLookaheads = states[stateId].lookaheads[kernel]
        
        for link in propagations:
          let targetState = link.targetStateId
          let targetItem = link.targetItem
          
          if targetItem notin states[targetState].lookaheads:
            states[targetState].lookaheads[targetItem] = initBitSet(sourceLookaheads.len)
          
          # Union source lookaheads into target
          if states[targetState].lookaheads[targetItem].union(sourceLookaheads):
            changed = true
  
  debugEchoMsg "[Pager] Propagation converged after ", iterations, " iterations"

# Convert Pager's LR0State format to StateKernels format
proc convertPagerToStateKernels(
  pagerStates: seq[LR0State],
  ctx: SymbolContext
): seq[StateKernels] =
  ## Convert from Pager's format to existing StateKernels format
  debugEchoMsg "[Pager] Converting to StateKernels format..."
  
  result = newSeq[StateKernels](pagerStates.len)
  
  for i, pagerState in pagerStates:
    result[i] = stdtables.initTable[CoreItem, LookaheadSet]()
    
    for kernel in pagerState.kernels:
      let core = CoreItem(
        variableIndex: kernel.variableIndex,
        productionIndex: kernel.productionIndex,
        position: kernel.position,
        inheritedPrecedence: kernel.inheritedPrecedence.int16
      )
      
      # Get lookaheads for this kernel
      if kernel in pagerState.lookaheads:
        result[i][core] = pagerState.lookaheads[kernel]
      else:
        result[i][core] = initBitSet(ctx.maxIndex)
  
  debugEchoMsg "[Pager] Conversion complete: ", result.len, " states"

# TODO: Integration point in buildParseTable with -d:usePagerLALR

proc buildParseTable*(grammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar): BuildParseTable =
  ## Builds the LR(1) parse table from the syntax grammar.
  ##
  ## This is the core of the parser generator - it constructs the parse table
  ## that drives the GLR parser at runtime.
  ##
  ## Algorithm:
  ## 1. **Grammar Augmentation**: Add S' -> S to create a unique accept state
  ## 2. **Nullability Analysis**: Compute which non-terminals can derive ε
  ## 3. **FIRST Set Computation**: Calculate lookahead sets
  ## 4. **LR State Construction**: Build canonical LR(1) item sets
  ##    - Uses LALR optimization if USE_LALR_OPTIMIZED is true
  ##    - Otherwise uses full canonical LR(1)
  ## 5. **Action/GOTO Construction**: Populate parse table from item sets
  ## 6. **Conflict Resolution**: Apply precedence and associativity rules
  ## 7. **Minimization**: Apply state merging to reduce table size
  ##
  ## The resulting table maps (state, lookahead) -> action (shift/reduce/accept)
  ## and (state, non-terminal) -> goto state.
  ##
  ## Complexity: O(n² * t) for LALR, O(n³ * t) for canonical LR(1)
  ##   where n = grammar size, t = number of terminals
  debugEchoMsg "Building parse table..."

  ## Build the LR(1) parse table from the grammar
  
  # Augment grammar with S' -> S rule to ensure proper reduction of start symbol
  var augmentedGrammar = grammar
  
  # --- Helper to resolve names ---
  proc getSymbolName(sym: GrammarSymbol): string =
    case sym.kind
    of stNonTerminal:
      if sym.index >= 0 and sym.index.int < augmentedGrammar.variables.len:
        return augmentedGrammar.variables[sym.index].name
      return "non_terminal(" & $sym.index & ")"
    of stExternal:
      if sym.index >= 0 and sym.index.int < augmentedGrammar.externalTokens.len:
        return augmentedGrammar.externalTokens[sym.index].name
      return "external(" & $sym.index & ")"
    of stTerminal:
      if sym.index >= 0 and sym.index.int < lexicalGrammar.variables.len:
        return lexicalGrammar.variables[sym.index].name
      return "terminal(" & $sym.index & ")"
    of stEnd:
      return "EOF"
    else:
      return $sym

  # Add _augmented_start variable at the end
  let startSymbol = GrammarSymbol(kind: stNonTerminal, index: 0)
  
  let augmentedStartProdStep = ProductionStep(
    symbol: startSymbol,
    precedence: nonePrecedence(),
    associativity: none(GrammarAssociativity),
    alias: none(Alias),
    fieldName: none(string),
    reservedWordSetId: NoReservedWords
  )
  
  let augmentedStartProd = Production(
    steps: @[augmentedStartProdStep],
    dynamicPrecedence: 0,
    precedence: 0,
    associativity: none(GrammarAssociativity)
  )
  
  augmentedGrammar.variables.add(SyntaxVariable(
    name: "_augmented_start", 
    kind: vtHidden, 
    productions: @[augmentedStartProd]
  ))
  let augmentedStartIndex = augmentedGrammar.variables.high

  debugEchoMsg "Computing nullability..."
  computeNullability(augmentedGrammar)

  let firstSets = computeFirst(augmentedGrammar)
  
  # Build LR states - choose algorithm based on compile flags
  when USE_PAGER_LALR or defined(usePagerLALR):
    # === Pager's LALR Algorithm ===
    echo "[Treestand] Using Pager's LALR algorithm (propagation-based)"
    
    # Step 1: Build LR(0) automaton
    let (lr0States, lr0Transitions) = buildLR0Automaton(
      augmentedGrammar,
      augmentedStartIndex
    )
    
    # Step 2: Compute propagation graph
    let ctx = newSymbolContext(lexicalGrammar.variables.len, augmentedGrammar.externalTokens.len)
    var statesWithProp = computeLookaheadPropagations(
      lr0States,
      lr0Transitions,
      augmentedGrammar,
      lexicalGrammar,
      firstSets,
      ctx
    )
    
    # Step 3: Propagate lookaheads to fixed point
    propagateLookaheads(statesWithProp)
    
    # Step 4: Convert to StateKernels format
    let statesLALR = convertPagerToStateKernels(statesWithProp, ctx)
    
    # # DEBUG: Show lookaheads for each state
    # debugEchoMsg "[Pager] Lookaheads per state:"
    # for i in 0 ..< min(statesLALR.len, 5):  # Show first 5 states
    #   debugEchoMsg "  State ", i, ":"
    #   for core, lookaheads in statesLALR[i]:
    #     let lookaheadSyms = block:
    #       var syms: seq[string] = @[]
    #       for bit in lookaheads:
    #         syms.add($ctx.bitToSymbol(bit))
    #       syms
    #     debugEchoMsg "    ", core, " -> lookaheads: ", lookaheadSyms
    
    # Step 5: Convert to full LR1 items for table building
    let closureCache = precomputeClosureCache(augmentedGrammar, lexicalGrammar, firstSets)
    var states = newSeq[HashSet[LR1Item]](statesLALR.len)
    for i, kernels in statesLALR:
      let fullClosure = getTransitiveClosure(augmentedGrammar, kernels, closureCache, firstSets)
      states[i] = convertStateKernelsToLR1Items(fullClosure, ctx)
    
    let transitions = lr0Transitions
    
  elif USE_LALR_OPTIMIZED:
    echo "[Treestand] Using optimized LALR(1) implementation"
    # Call new LALR implementation
    let (statesLALR, _, transitionsLALR) = buildCanonicalCollectionLALR(
      augmentedGrammar,
      lexicalGrammar,
      firstSets,
      augmentedStartIndex
    )
    
    # Precompute closure cache (needed for converting kernels to full states)
    let closureCache = precomputeClosureCache(augmentedGrammar, lexicalGrammar, firstSets)
   
    # Convert StateKernels to HashSet[LR1Item] for compatibility
    # IMPORTANT: buildParseTable needs FULL CLOSURE, not just kernels
    var states = newSeq[HashSet[LR1Item]](statesLALR.len)
    let ctx = newSymbolContext(lexicalGrammar.variables.len, augmentedGrammar.externalTokens.len)
    for i, kernels in statesLALR:
      # Compute full closure for this state's kernels
      let fullClosure = getTransitiveClosure(augmentedGrammar, kernels, closureCache, firstSets)
      # Convert full closure to LR1Items
      states[i] = convertStateKernelsToLR1Items(fullClosure, ctx)
    
    let transitions = transitionsLALR
  else:
    echo "[Treestand] Using traditional Canonical LR(1) implementation"
    # Pass augmentedStartIndex to buildCanonicalCollection so it knows where to start
    let (states, _, transitions) = buildCanonicalCollection(augmentedGrammar, firstSets, augmentedStartIndex)
  
  # --- Helper: Find shortest symbol path to a state (BFS) ---
  proc findPathToState(targetState: int): seq[GrammarSymbol] =
    var queue = initDeque[int]()
    queue.addLast(0)
    
    var parents = initTable[int, tuple[parent: int, sym: GrammarSymbol]]()
    var visited = initHashSet[int]()
    visited.incl(0)
    
    var found = false
    while queue.len > 0:
      let curr = queue.popFirst()
      if curr == targetState:
        found = true
        break
        
      if curr < transitions.len:
        for sym, nextState in transitions[curr]:
          if nextState notin visited:
            visited.incl(nextState)
            parents[nextState] = (parent: curr, sym: sym)
            queue.addLast(nextState)
    
    var path = newSeq[GrammarSymbol]()
    if found and targetState != 0:
      var curr = targetState
      while curr != 0:
        if curr in parents:
          let p = parents[curr]
          path.add(p.sym)
          curr = p.parent
        else:
          break
      path.reverse()
    return path

  var entries = newSeq[BuildParseTableEntry](states.len)
  
  # Initialize parse table entries
  for i in 0 ..< states.len:
    entries[i] = BuildParseTableEntry(actionMap: @[], gotoMap: @[])
  
  # Fill in the parse table
  for stateId in 0 ..< states.len:
    let itemSet = states[stateId]
    

    
    for item in itemSet:
      let variable = augmentedGrammar.variables[item.variableIndex]
      let production = variable.productions[item.productionIndex]
      
      if item.position < production.steps.len:
        # --- SHIFT LOGIC ---
        let nextSym = production.steps[item.position].symbol
        
        let effectiveRulePrec = if production.precedence != 0: production.precedence else: item.inheritedPrecedence
        
        let shiftPrecVal = if item.position > 0:
            let prevStepPrec = production.steps[item.position - 1].precedence
            if prevStepPrec.kind == pkInteger: prevStepPrec.intValue else: effectiveRulePrec
          else:
            effectiveRulePrec
            
        if stateId < transitions.len and nextSym in transitions[stateId]:
          let gotoStateId = transitions[stateId][nextSym]
          
          if nextSym.kind != stNonTerminal:
            # let newAction = BuildParseAction(
            #   kind: bpakShift,
            #   participants: @[],
            #   shiftState: gotoStateId.uint32,
            #   shiftPrecedence: shiftPrecVal,
            #   shiftDynamicPrecedence: production.dynamicPrecedence
            # )

            var alreadyExists = false
            for (sym, action) in entries[stateId].actionMap:
               if sym == nextSym and action.kind == bpakShift and action.shiftState == gotoStateId.uint32 and action.shiftPrecedence == shiftPrecVal and action.shiftDynamicPrecedence == production.dynamicPrecedence:
                 alreadyExists = true
                 break
            
            if not alreadyExists:
                entries[stateId].actionMap.add((
                  sym: nextSym,
                  action: BuildParseAction(
                    kind: bpakShift,
                    participants: @[],
                    shiftState: gotoStateId.uint32,
                    shiftPrecedence: shiftPrecVal,
                    shiftDynamicPrecedence: production.dynamicPrecedence
                  )
                ))
          else:
            var alreadyExists = false
            for g in entries[stateId].gotoMap:
               if g.sym == nextSym: 
                 alreadyExists = true
                 break
            if not alreadyExists:
              entries[stateId].gotoMap.add((sym: nextSym, state: gotoStateId.uint32))

      else:
        # --- REDUCE LOGIC ---
        if item.variableIndex == augmentedStartIndex:
          entries[stateId].actionMap.add((
            sym: item.lookahead,
            action: BuildParseAction(kind: bpakAccept)
          ))
        else:
          let lhs = GrammarSymbol(kind: stNonTerminal, index: item.variableIndex.uint16)
          
          let reduceAction = BuildParseAction(
              kind: bpakReduce,
              participants: @[lhs],
              reduceSymbol: lhs,
              reduceCount: production.steps.len.uint32,
              reducePrecedence: production.dynamicPrecedence,
              reduceStaticPrecedence: production.precedence,
              reduceAssociativity: production.associativity
          )

          var alreadyExists = false
          for existing in entries[stateId].actionMap:
             if existing.sym == item.lookahead and existing.action == reduceAction:
                alreadyExists = true
                break
          
          if not alreadyExists:
             entries[stateId].actionMap.add((
               sym: item.lookahead, 
               action: reduceAction
             ))


  
  var productionInfos = newSeq[BuildProductionInfo]()
  for i in 0 ..< grammar.variables.len:
    let variable = grammar.variables[i]
    for production in variable.productions:
      var fieldNames: seq[string] = @[]
      for step in production.steps:
        if step.fieldName.isSome():
          fieldNames.add(step.fieldName.get())
        else:
          fieldNames.add("")
      
      productionInfos.add(BuildProductionInfo(
        symbol: GrammarSymbol(kind: stNonTerminal, index: i.uint16),
        fieldCount: fieldNames.len.uint32,
        childCount: production.steps.len.uint32,
        fieldNames: fieldNames
      ))
  
  for stateIdx in 0 ..< entries.len:
    for extra in grammar.extraSymbols:
      if extra.kind == stExternal or extra.kind == stTerminal:
        var alreadyHasAction = false
        for (sym, _) in entries[stateIdx].actionMap:
          if sym == extra:
            alreadyHasAction = true
            break
        
        if not alreadyHasAction:
          entries[stateIdx].actionMap.add((
            sym: extra,
            action: BuildParseAction(kind: bpakShiftExtra)
          ))
  
  # Detect conflicts in the parse table
  for stateId in 0..<entries.len:
    proc isConflictExpected(ruleSymbols: seq[GrammarSymbol]): bool =
      if ruleSymbols.len == 0: return true
      for expectedSet in augmentedGrammar.expectedConflicts:
        var allFound = true
        for rule in ruleSymbols:
          var ruleFound = false
          for expectedRule in expectedSet:
            if rule == expectedRule:
              ruleFound = true
              break
          if not ruleFound:
            allFound = false
            break
        if allFound: 
           debugEchoMsg "Conflict expected and suppressed for: ", ruleSymbols
           return true
      debugEchoMsg "Conflict NOT expected for: ", ruleSymbols
      debugEchoMsg "Available expected sets:"
      for s in augmentedGrammar.expectedConflicts:
        debugEchoMsg "  Set: ", s
      return false

    var actionsBySymbol = stdtables.initTable[GrammarSymbol, seq[BuildParseAction]]()
    
    for (sym, action) in entries[stateId].actionMap:
      if sym notin actionsBySymbol:
        actionsBySymbol[sym] = @[]
      actionsBySymbol[sym].add(action)
    var resolvedActions: seq[tuple[sym: GrammarSymbol, action: BuildParseAction]] = @[]

    for sym, actions in actionsBySymbol:
      var shiftActions: seq[BuildParseAction] = @[]
      var reduces: seq[BuildParseAction] = @[]
      
      for action in actions:
        case action.kind
        of bpakShift, bpakShiftExtra:
          shiftActions.add(action)
        of bpakReduce:
          reduces.add(action)
        else:
          discard

      # Deduplication of shift actions removed to allow conflict detection to see 
      # all potential shifts, especially for Shift/Reduce/Shift conflicts where
      # different shifts might have different precedences relative to a reduce.
      # The conflict resolution logic later will handle choosing the best shift 
      # if they are compatible, or report a conflict if they are not.

      var processed = false

      if shiftActions.len > 0 and reduces.len > 0:
          # Shift/Reduce conflict
          var allResolved = true
          var winningReduces: seq[BuildParseAction] = @[]
          var keepShifts = false
          
          # Find winning reduces first
          for r in reduces:
            let reducePrecedence = r.reduceStaticPrecedence
            var shiftIsMore = false
            var shiftIsLess = false
            
            for shift in shiftActions:
               let shiftPrecedence = shift.shiftPrecedence
               # debugEchoMsg "Conflict: Shift(", shift.shiftPrecedence, ") vs Reduce(", reducePrecedence, ") for symbol ", getSymbolName(sym)
               if shiftPrecedence > reducePrecedence:
                 shiftIsMore = true
               elif shiftPrecedence < reducePrecedence:
                 shiftIsLess = true
             
            if shiftIsMore and not shiftIsLess:
              # Shift wins over this reduce
              keepShifts = true
            elif shiftIsLess and not shiftIsMore:
              # This reduce wins
              winningReduces.add(r)
              
              # HEURISTIC: If Shift lost because it has negative precedence vs default (0),
              # keep it anyway to allow GLR to resolve potential invalid reductions.
              if shiftActions.len == 1 and shiftActions[0].shiftPrecedence < 0 and reducePrecedence == 0:
                keepShifts = true
            elif not shiftIsLess and not shiftIsMore:
               # Precedence equal, check associativity
               var resolved = false
               if r.reduceAssociativity.isSome:
                  let assoc = r.reduceAssociativity.get
                  if assoc == gaLeft:
                     # Associativity: Left -> Reduce wins
                     winningReduces.add(r)
                     resolved = true
                  elif assoc == gaRight:
                     # Associativity: Right -> Shift wins
                     keepShifts = true
                     resolved = true
               if not resolved:
                  allResolved = false
            else:
               allResolved = false
          
          if not allResolved:
            var participants: seq[GrammarSymbol] = @[]
            for a in actions:
              for p in a.participants:
                if p notin participants: participants.add(p)
            
            if not isConflictExpected(participants):
              # --- Path Reconstruction for Context ---
              let path = findPathToState(stateId)
              var contextStr = ""
              for s in path:
                contextStr &= getSymbolName(s) & " "
              contextStr &= " •  " & getSymbolName(sym) & "  ..."

              var conflictMsg = "Unresolved conflict for symbol sequence:\n\n"
              conflictMsg &= "  " & contextStr & "\n\n"
              conflictMsg &= "Possible interpretations:\n\n"
              for i, s in shiftActions:
                 conflictMsg &= "  " & $(i+1) & ":  SHIFT " & getSymbolName(sym) & " (precedence: " & $s.shiftPrecedence & ")\n"
              
              let baseIdx = shiftActions.len + 1
              for i, r in reduces:
                conflictMsg &= "  " & $(baseIdx+i) & ":  REDUCE using rule " & getSymbolName(r.reduceSymbol) & 
                               " (precedence: " & $r.reduceStaticPrecedence & 
                               ", assoc: " & (if r.reduceAssociativity.isSome: $r.reduceAssociativity.get else: "none") & ")\n"
              conflictMsg &= "\nPossible resolutions:\n\n"
              conflictMsg &= "  1:  Specify a higher precedence in the reduce rules\n"
              conflictMsg &= "  2:  Add a conflict declaration for these rules\n"
              
              raise newException(BuildTablesError, conflictMsg)
            else:
               # Conflict expected - keep all actions (GLR)
               for a in actions: resolvedActions.add((sym: sym, action: a))
               processed = true
          
          else:
             # All resolved! Add winners
             if keepShifts:
                # Keep highest precedence shift(s)
                var bestShiftPrec = int32.low
                for s in shiftActions:
                   if s.shiftPrecedence > bestShiftPrec: bestShiftPrec = s.shiftPrecedence
                for s in shiftActions:
                   if s.shiftPrecedence == bestShiftPrec:
                     resolvedActions.add((sym: sym, action: s))
             
             for r in winningReduces:
                resolvedActions.add((sym: sym, action: r))
             processed = true

      elif reduces.len > 1:
          # Reduce/Reduce conflict
          var bestPrecedence = int32.low
          var bestCount = 0
          
          # Find max precedence
          for r in reduces:
              let currentPrec = r.reduceStaticPrecedence
              if currentPrec > bestPrecedence:
                bestPrecedence = currentPrec
                bestCount = 1
              elif currentPrec == bestPrecedence:
                bestCount += 1
          
          # Identify winners based on precedence
          var precedenceWinners: seq[BuildParseAction] = @[]
          for r in reduces:
              if r.reduceStaticPrecedence == bestPrecedence:
                  precedenceWinners.add(r)

          if precedenceWinners.len > 1:
            var isExpected = false
            
            var allPairsExpected = true
            if precedenceWinners.len > 1:
               for idx1 in 0 ..< precedenceWinners.len:
                 for idx2 in (idx1 + 1) ..< precedenceWinners.len:
                   let sym1 = precedenceWinners[idx1].reduceSymbol
                   let sym2 = precedenceWinners[idx2].reduceSymbol
                   
                   var pairFound = false
                   for conflictSet in augmentedGrammar.expectedConflicts:
                      if sym1 in conflictSet and sym2 in conflictSet:
                        pairFound = true
                        break
                   if not pairFound:
                     allPairsExpected = false
                     break
                 if not allPairsExpected: break
            
            if allPairsExpected:
               isExpected = true
               
            if not isExpected:
              # --- Path Reconstruction for Context ---
              let path = findPathToState(stateId)
              var contextStr = ""
              for s in path:
                contextStr &= getSymbolName(s) & " "
              contextStr &= " •  " & getSymbolName(sym) & "  ..."

              var conflictMsg = "Unresolved conflict for symbol sequence:\n\n"
              conflictMsg &= "  " & contextStr & "\n\n"
              conflictMsg &= "Possible interpretations:\n\n"
              for i, r in reduces:
                conflictMsg &= "  " & $(i+1) & ":  REDUCE using rule " & getSymbolName(r.reduceSymbol) & 
                               " (precedence: " & $r.reduceStaticPrecedence & 
                               ", dynamic: " & $r.reducePrecedence & ")\n"
              conflictMsg &= "\nPossible resolutions:\n\n"
              conflictMsg &= "  1:  Specify different precedence levels for conflicting rules\n"
              conflictMsg &= "  2:  Restructure the grammar to avoid ambiguity\n"
              
              raise newException(BuildTablesError, conflictMsg)
            else:
               # Expected conflict: Keep conflicting reduces (GLR)
               for r in precedenceWinners:
                  resolvedActions.add((sym: sym, action: r))
               processed = true
          else:
             # Winner found by precedence
             resolvedActions.add((sym: sym, action: precedenceWinners[0]))
             processed = true
      
      if not processed:
          # Single action or multiple shifts (ambiguity? or just duplicates?)
          # If multiple shifts, they should be identical due to deduplication during add
          # unless we added shifts with different precedences but no reduces?
          # If multiple shifts with different precedences but SAME state/sym, deduplication preserved only one? 
          # No, I removed deduplication. So we might have multiple shifts.
          # If we have multiple shifts, we should pick best one?
          # Actually multiple shifts for same symbol usually implies different precedence or dynamic prec?
          # If reduces.len == 0, and shiftActions.len > 1:
          if shiftActions.len > 1:
             # Pick best shift
             var bestShiftPrec = int32.low
             for s in shiftActions:
                if s.shiftPrecedence > bestShiftPrec: bestShiftPrec = s.shiftPrecedence
             for s in shiftActions:
                if s.shiftPrecedence == bestShiftPrec:
                  resolvedActions.add((sym: sym, action: s))
          elif shiftActions.len == 1:
             resolvedActions.add((sym: sym, action: shiftActions[0]))
          elif reduces.len == 1:
             resolvedActions.add((sym: sym, action: reduces[0]))
          else:
             # 0 actions? Should not happen as we loop over map
             for a in actions: resolvedActions.add((sym: sym, action: a))

    # Update actions
    entries[stateId].actionMap = resolvedActions
  
  var resultTable = BuildParseTable(
    entries: entries,
    productionInfos: productionInfos,
    externalSymbols: @[]
  )
  
  minimizeParseTable(resultTable, grammar, lexicalGrammar)
  resultTable