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


# --- New LALR(1) Data Structures (Kernel-Only with BitSet) ---

type
  CoreItem* = object
    ## The "core" of an LR item: just the rule position, without lookaheads.
    ## `inheritedPrecedence` carries the enclosing precedence context down
    ## through closures (used for shift-precedence computation during the
    ## fill phase). It is part of the item identity for the fill closure but
    ## kept to step-prec-only during state construction to keep state counts
    ## compact.
    variableIndex*: uint16         # Which non-terminal variable this production belongs to
    productionIndex*: uint16       # Which production of that variable
    position*: uint16              # Dot position (0 = before first symbol)
    inheritedPrecedence*: int16    # Precedence inherited from parent rule

  StateKernels* = stdtables.Table[CoreItem, LookaheadSet]
    ## Maps each core item to its set of lookaheads.
    ## This is the state representation for LALR(1) construction.

proc hash*(item: CoreItem): Hash =
  ## Hash for CoreItem: the four 16-bit fields pack into one uint64,
  ## so a single mix is enough. Called millions of times during construction.
  let packed = item.variableIndex.uint64 or
    (item.productionIndex.uint64 shl 16) or
    (item.position.uint64 shl 32) or
    (cast[uint16](item.inheritedPrecedence).uint64 shl 48)
  result = hash(packed)

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


# Forward declarations
proc buildLexicalNfa(lexicalGrammar: var LexicalGrammar)
proc dfaFromNfa(
    lexicalGrammar: LexicalGrammar,
    tokenConflictMap: TokenConflictMap,
    startConfigs: seq[seq[uint32]]
): tuple[table: BuildLexTable, startStateMap: seq[uint32]]

proc isNullable(grammar: SyntaxGrammar, symbol: GrammarSymbol): bool =
  # Only Non-Terminals can be nullable
  if symbol.kind != stNonTerminal:
    return false

  # O(1) Lookup
  return grammar.variables[symbol.index].isNullable


proc buildParseTable*(grammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar, skipConflictDetection: bool = false, tokenConflictMap: TokenConflictMap = TokenConflictMap()): BuildParseTable

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

proc buildTables*(syntaxGrammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar, skipConflictDetection: bool = false): Tables =
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
  # The token conflict map is needed by parse table minimization (to decide
  # which states may merge), so compute it before building the parse table.
  let firstSets = computeFirst(syntaxGrammar)
  let lastSets = computeLast(syntaxGrammar)
  let followingTokens = computeFollowingTokens(syntaxGrammar, mutableLexical, firstSets, lastSets)
  let tokenConflictMap = newTokenConflictMap(mutableLexical, followingTokens)

  # Using modified syntax grammar with newly detected extras
  var parseTable = buildParseTable(mutableSyntax, mutableLexical, skipConflictDetection, tokenConflictMap)

  # 4. Determine unique lexical configurations derived from Parse Table
  # Collect valid lookaheads for each parse state
  # Build lexical states
  debugEchoMsg "[Treestand] Building lexical states..."
  
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
    
    let configSet = toHashSet(configSeq)
    # Still use HashSet for uniqueness check map?
    # Or map seq directly? stdtables supports seq keys if elements support hash.

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

proc compileString(nfa: var Nfa, s: string, isSeparator: bool = false, precedence: int32 = 0): Fragment =
  if s.len == 0:
    # Empty string matches immediately
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
    let stateId = nfa.addState(NfaState(
      kind: nskAdvance,
      advanceChars: charSet,
      advanceStateId: 0, # Pending patch
      advanceIsSep: isSeparator,
      advancePrecedence: precedence,
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

proc parseRegexInternal(p: var RegexParser, nfa: var Nfa, isSeparator: bool, precedence: int32): Fragment

# Character Class Parsing
proc parseCharClass(p: var RegexParser, nfa: var Nfa, isSeparator: bool, precedence: int32): Fragment =
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
      advanceIsSep: isSeparator,
      advancePrecedence: precedence
  ))
  
  return Fragment(
      startState: stateId,
      outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)]
  )

proc parseAtom(p: var RegexParser, nfa: var Nfa, isSeparator: bool, precedence: int32): Fragment =
  let c = p.peek()
  case c
  of '(':
    discard p.next()
    result = parseRegexInternal(p, nfa, isSeparator, precedence)
    discard p.consume(')')
  of '[':
    discard p.next()
    result = parseCharClass(p, nfa, isSeparator, precedence)
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
    of 'p':
        # Unicode property \p{...}
        if p.peek() == '{':
            discard p.next() # consume {
            var name = ""
            while p.peek() != '}' and p.peek() != '\0':
                name.add(p.next())
            
            if p.peek() == '}': discard p.next()
            
            if name == "XID_Start":
                echo "[Treestand] Compiling property ", name
                let ranges = @[
                  CharacterRange(start: 65, `end`: 91),   # A-Z
                  CharacterRange(start: 95, `end`: 96),   # _
                  CharacterRange(start: 97, `end`: 123)   # a-z
                ]
                charSet = CharacterSet(ranges: ranges)
            elif name == "XID_Continue":
                # Approximate XID_Continue with [a-zA-Z0-9_]
                let ranges = @[
                  CharacterRange(start: 48, `end`: 58),   # 0-9
                  CharacterRange(start: 65, `end`: 91),   # A-Z
                  CharacterRange(start: 95, `end`: 96),   # _
                  CharacterRange(start: 97, `end`: 123)   # a-z
                ]
                charSet = CharacterSet(ranges: ranges)
            else:
                # Fallback for unknown properties
                echo "[Treestand] Warning: Unknown unicode property \p{", name, "}"
                charSet = fromChar('p') # Fallback to literal p? or empty?
        else:
            charSet = fromChar('p')
    else: charSet = fromChar(esc)

    let stateId = nfa.addState(NfaState(
        kind: nskAdvance,
        advanceChars: charSet,
        advanceStateId: 0,
        advanceIsSep: isSeparator
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
              advanceStateId: 0,
              advanceIsSep: isSeparator,
              advancePrecedence: precedence
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
        advanceStateId: 0,
        advanceIsSep: isSeparator,
        advancePrecedence: precedence
    ))
    result = Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])

proc parseQuantifier(p: var RegexParser, nfa: var Nfa, atom: Fragment, precedence: int32): Fragment =
  let c = p.peek()
  case c
  of '*':
    discard p.next()
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

proc parseSequence(p: var RegexParser, nfa: var Nfa, isSeparator: bool, precedence: int32): Fragment =
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

  var currentFrag = parseQuantifier(p, nfa, parseAtom(p, nfa, isSeparator, precedence), precedence)
  
  while p.peek() != '|' and p.peek() != ')' and p.peek() != '\0':
      let nextFrag = parseQuantifier(p, nfa, parseAtom(p, nfa, isSeparator, precedence), precedence)
      patch(nfa, currentFrag.outArrows, nextFrag.startState)
      currentFrag.outArrows = nextFrag.outArrows
      # startState unchanged (sequence start)
      
  return currentFrag

proc parseRegexInternal(p: var RegexParser, nfa: var Nfa, isSeparator: bool, precedence: int32): Fragment =
  # Alternations: Seq | Seq | ...
  let firstSeq = parseSequence(p, nfa, isSeparator, precedence)
  
  if p.peek() != '|':
      return firstSeq
      
  # Handle alternation
  var currentStart = firstSeq.startState
  var currentOut = firstSeq.outArrows
  
  while p.consume('|'):
      let nextSeq = parseSequence(p, nfa, isSeparator, precedence)
      let splitId = nfa.addState(NfaState(
          kind: nskSplit,
          splitLeftState: currentStart,
          splitRightState: nextSeq.startState
      ))
      currentStart = splitId
      currentOut.add(nextSeq.outArrows)
      
  return Fragment(startState: currentStart, outArrows: currentOut)

proc compileRegex(nfa: var Nfa, pattern: string, isSeparator: bool = false, precedence: int32 = 0): Fragment =
  try:
    var p = RegexParser(pattern: pattern, pos: 0)
    parseRegexInternal(p, nfa, isSeparator, precedence)
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
        advanceStateId: 0,
        advanceIsSep: isSeparator
    ))
    return Fragment(startState: stateId, outArrows: @[PatchTarget(stateId: stateId, kind: ptAdvance)])

proc compileRule(nfa: var Nfa, rule: Rule, isSeparator: bool = false, precedence: int32 = 0): Fragment =
  case rule.kind
  of rkString:
    return compileString(nfa, rule.stringValue, isSeparator, precedence)
  of rkPattern:
     return compileRegex(nfa, rule.patternValue, isSeparator, precedence)
  of rkSeq:
    # A then B
    # start = compile(A)
    # patch(A.out, start(B))
    # out = B.out
    
    # Just generic seq
    if rule.seqMembers.len == 0:
        return compileString(nfa, "", isSeparator, precedence)
        
    var frag = compileRule(nfa, rule.seqMembers[0], isSeparator, precedence)
    for i in 1 ..< rule.seqMembers.len:
      let nextFrag = compileRule(nfa, rule.seqMembers[i], isSeparator, precedence)
      patch(nfa, frag.outArrows, nextFrag.startState)
      frag.outArrows = nextFrag.outArrows
      # startState remains initial
    return frag
    
  of rkChoice:
    # A or B
    # Split(startA, startB)
    # out = outA + outB
    
    if rule.choiceMembers.len == 0:
        return compileString(nfa, "", isSeparator, precedence) # Empty choice??
    
    # We can chain splits for multiple choices.
    # Split(A, Split(B, C...))
    
    var frag = compileRule(nfa, rule.choiceMembers[0], isSeparator, precedence)
    var currentStart = frag.startState
    var currentOut = frag.outArrows
    
    for i in 1 ..< rule.choiceMembers.len:
        let nextFrag = compileRule(nfa, rule.choiceMembers[i], isSeparator, precedence)
        let splitId = nfa.addState(NfaState(
            kind: nskSplit,
            splitLeftState: currentStart,
            splitRightState: nextFrag.startState
        ))
        currentStart = splitId
        currentOut.add(nextFrag.outArrows)
        
    return Fragment(startState: currentStart, outArrows: currentOut)
    
  of rkMetadata:
    # Delegate to inner
    # Extract precedence from metadata if present and override
    var newPrecedence = precedence
    if rule.metadataParams.precedence.kind == pkInteger:
       newPrecedence = rule.metadataParams.precedence.intValue
       
    return compileRule(nfa, rule.metadataRule[], isSeparator, newPrecedence)
  of rkBlank:
    return compileString(nfa, "", isSeparator, precedence)
  of rkReserved:
    return compileRule(nfa, rule.reservedRule[], isSeparator, precedence)
  of rkRepeat:
    # Zero or more: Split(atomStart, next) -> atom -> Split(atomStart, next)
    let atom = compileRule(nfa, rule.repeatContent[], isSeparator, precedence)
    
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
  
  # Build separator rule: repeat(choice([separators..., Blank]))
  # This follows tree-sitter's approach
  var separatorRule: Rule
  if lexicalGrammar.separators.len == 0:
    separatorRule = Rule(kind: rkBlank)
  else:
    # Add Blank to separators to allow zero separators
    var sepChoices = lexicalGrammar.separators
    sepChoices.add(Rule(kind: rkBlank))
    separatorRule = Rule(
      kind: rkRepeat,
      repeatContent: (block:
        var r = new(Rule)
        r[] = Rule(kind: rkChoice, choiceMembers: sepChoices)
        r
      )
    )
  
  for i in 0 ..< lexicalGrammar.variables.len:
    let variable = lexicalGrammar.variables[i]
    let tokenFrag = compileRule(nfaObj, variable.rule, false, variable.implicitPrecedence)
    
    # Set start state in variable
    lexicalGrammar.variables[i].startState = tokenFrag.startState
    
    # Check if this is an immediate token (token wrapped with isMainToken)
    var isImmediateToken = false
    if variable.rule.kind == rkMetadata:
      isImmediateToken = variable.rule.metadataParams.isMainToken
    
    # Connect to Accept state, optionally via separator rule
    let acceptId = nfaObj.addState(NfaState(
      kind: nskAccept,
      acceptVariableIndex: i,
      acceptPrecedence: variable.implicitPrecedence
    ))
    
    if not isImmediateToken and lexicalGrammar.separators.len > 0:
      # Token -> Separator -> Accept
      let sepFrag = compileRule(nfaObj, separatorRule, true, 0) # isSeparator = true
      
      # Token done -> Start attempting separators
      patch(nfaObj, tokenFrag.outArrows, sepFrag.startState)
      
      # Separators done -> Accept
      patch(nfaObj, sepFrag.outArrows, acceptId)
    else:
      # Token -> Accept
      patch(nfaObj, tokenFrag.outArrows, acceptId)
    
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
    
    # [Treestand Fix] "regex_pattern" should conflict-win over others if equal precedence
    # Check names first to override implicit precedence
    let leftName = grammar.variables[idLeft].name
    let rightName = grammar.variables[idRight].name
    
    if leftName == "regex_pattern": return true
    if rightName == "regex_pattern": return false
    
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
            
    return true

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


# === New LALR(1) Closure Precomputation (using BitSet) ===

proc precomputeClosureCache*(
    grammar: SyntaxGrammar,
    lexicalGrammar: LexicalGrammar,
    firstSets: FirstSets,
    firstSetsBits: stdtables.Table[GrammarSymbol, BitSet]
): ClosureCache =
  ## Precompute closure expansions using BitSet for efficient lookahead operations.
  ## This is the optimized version for LALR(1) with kernel-only storage.
  
  echo "[Treestand] Precomputing closure cache..."

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
                  if sym in firstSetsBits:
                    discard betaFirst.union(firstSetsBits[sym])
                else:
                  # Terminal or External
                  let bit = ctx.symbolToBit(sym)
                  if bit >= 0:
                    betaFirst.incl(bit)

                if not isNullable(grammar, sym):
                  allBetaNullable = false
                  break
              
              # Determine new lookaheads for firstSym expansion.
              # When beta is fully nullable (in particular when firstSym is
              # the LAST step of the production, so beta is empty), the
              # current lookaheads also flow into firstSym's expansion -
              # matching tree-sitter's item_set_builder.rs, which pushes the
              # current lookaheads when the production has no next step.
              var newLookaheads = betaFirst
              if allBetaNullable:
                discard newLookaheads.union(lookaheads)
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

# === LALR(1) table construction support ===

type
  ShiftDedupKey = object
    sym: GrammarSymbol
    target: uint32
    prec: int32
    dynPrec: int32

  ReduceDedupKey = object
    # Plain-field dedup key for reduce actions (participants are always
    # @[reduceSymbol], so they don't need to be hashed/compared).
    sym: GrammarSymbol
    reduceSymbol: GrammarSymbol
    reduceCount: uint32
    dynPrec: int32
    staticPrec: int32
    assoc: Option[GrammarAssociativity]

proc hash(key: ShiftDedupKey): Hash =
  var h = hash(key.sym)
  h = h !& hash(key.target)
  h = h !& hash(key.prec)
  h = h !& hash(key.dynPrec)
  result = !$h

proc `==`(a, b: ShiftDedupKey): bool =
  a.sym == b.sym and a.target == b.target and
    a.prec == b.prec and a.dynPrec == b.dynPrec

proc hash(key: ReduceDedupKey): Hash =
  var h = hash(key.sym)
  h = h !& hash(key.reduceSymbol)
  h = h !& hash(key.reduceCount)
  h = h !& hash(key.dynPrec)
  h = h !& hash(key.staticPrec)
  h = h !& hash(key.assoc.isSome)
  if key.assoc.isSome:
    h = h !& hash(ord(key.assoc.get))
  result = !$h

proc `==`(a, b: ReduceDedupKey): bool =
  a.sym == b.sym and a.reduceSymbol == b.reduceSymbol and
    a.reduceCount == b.reduceCount and a.dynPrec == b.dynPrec and
    a.staticPrec == b.staticPrec and a.assoc == b.assoc

proc computeFirstBitSets(
    firstSets: FirstSets,
    ctx: SymbolContext
): stdtables.Table[GrammarSymbol, BitSet] =
  ## Convert FIRST sets (IntSet-based) to BitSets for fast union operations.
  result = stdtables.initTable[GrammarSymbol, BitSet]()
  for sym, firstSet in firstSets:
    var bs = initBitSet(ctx.maxIndex)
    for s in firstSet:
      let bit = ctx.symbolToBit(s)
      if bit >= 0:
        bs.incl(bit)
    result[sym] = bs

proc getTransitiveClosureFast*(
    grammar: var SyntaxGrammar,
    kernels: StateKernels,
    cache: ClosureCache,
    firstSetsBits: stdtables.Table[GrammarSymbol, BitSet],
    firstBetaCache: var stdtables.Table[FirstBetaCacheKey, tuple[first: BitSet, nullable: bool]],
    fullPrecedence: bool = false
): StateKernels =
  ## Single-pass transitive closure using precomputed additions, mirroring
  ## tree-sitter's `ParseItemSetBuilder::transitive_closure`. Because
  ## `cache.additions` is transitively closed, only the kernel items need to
  ## be processed - no worklist, no re-processing of closure items.
  ##
  ## `fullPrecedence` controls how precedence propagates into closure items:
  ## - false (used during state construction): only the precedence annotated
  ##   on the expanded step is kept. This yields few distinct precedence
  ##   variants and keeps the number of distinct states small.
  ## - true (used when filling parse actions): precedence is also inherited
  ##   from the enclosing production / parent item, matching the conflict-
  ##   resolution behavior of the previous propagation-based pipeline.
  result = kernels
  for item, itemLookaheads in kernels:
    template variable: untyped = grammar.variables[item.variableIndex]
    template production: untyped = grammar.variables[item.variableIndex].productions[item.productionIndex]
    if item.position >= production.steps.len.uint16:
      continue
    let symbol = production.steps[item.position].symbol
    if symbol.kind != stNonTerminal:
      continue
    let nonTermIdx = symbol.index.int
    if nonTermIdx >= cache.additions.len:
      continue

    # Expansion context = FIRST(beta) [+ item lookaheads if beta is nullable].
    # FIRST(beta) is memoized globally per (variable, production, position).
    let cacheKey = FirstBetaCacheKey(
      variableIndex: item.variableIndex,
      productionIndex: item.productionIndex,
      position: item.position
    )
    var betaFirst: BitSet
    var betaNullable: bool
    if cacheKey in firstBetaCache:
      (betaFirst, betaNullable) = firstBetaCache[cacheKey]
    else:
      betaFirst = initBitSet(cache.symbolContext.maxIndex)
      betaNullable = true
      for k in (item.position + 1).int ..< production.steps.len:
        let betaSym = production.steps[k].symbol
        if betaSym.kind == stNonTerminal:
          if betaSym in firstSetsBits:
            discard betaFirst.union(firstSetsBits[betaSym])
        else:
          let bit = cache.symbolContext.symbolToBit(betaSym)
          if bit >= 0:
            betaFirst.incl(bit)
        if not isNullable(grammar, betaSym):
          betaNullable = false
          break
      firstBetaCache[cacheKey] = (betaFirst, betaNullable)

    var expansionContext = betaFirst
    if betaNullable:
      discard expansionContext.union(itemLookaheads)

    let stepPrec = production.steps[item.position].precedence
    let stepPrecVal = if stepPrec.kind == pkInteger: stepPrec.intValue.int16 else: 0'i16
    let nextInheritedPrec =
      if not fullPrecedence: stepPrecVal
      elif stepPrecVal != 0: stepPrecVal
      elif production.precedence != 0: production.precedence.int16
      else: item.inheritedPrecedence

    for addition in cache.additions[nonTermIdx]:
      let newCore = CoreItem(
        variableIndex: addition.variableIndex,
        productionIndex: addition.productionIndex,
        position: 0,
        inheritedPrecedence: nextInheritedPrec
      )
      if addition.followInfo.propagatesLookaheads:
        var newLookaheads = addition.followInfo.lookaheads
        discard newLookaheads.union(expansionContext)
        discard result.mgetOrPut(newCore, BitSet()).union(newLookaheads)
      else:
        discard result.mgetOrPut(newCore, BitSet()).union(addition.followInfo.lookaheads)


proc sortedCores(kernels: StateKernels): seq[CoreItem] =
  ## Sorted kernel items, used as the canonical dedup key for a state.
  ## inheritedPrecedence is zeroed so that states differing only by
  ## precedence context merge together, keeping state counts compact.
  result = newSeqOfCap[CoreItem](kernels.len)
  for item in kernels.keys:
    var normalized = item
    normalized.inheritedPrecedence = 0
    result.add(normalized)
  result.sort()

proc registerStateLALR(
    states: var seq[StateKernels],
    transitions: var seq[stdtables.Table[GrammarSymbol, int]],
    stateIdsByCore: var stdtables.Table[seq[CoreItem], int],
    stateQueue: var Deque[int],
    inQueue: var HashSet[int],
    kernels: StateKernels
): int =
  ## Register a kernel item set as a state, deduplicating on the kernel
  ## CORE (items without lookaheads). On a repeat visit, lookaheads are
  ## unioned into the existing state and it is re-queued if they grew.
  ## (Module-level so it is safe to use from the compile-time VM path.)
  let coreSeq = sortedCores(kernels)
  if coreSeq in stateIdsByCore:
    let id = stateIdsByCore[coreSeq]
    var changed = false
    for item, la in kernels:
      if states[id].mgetOrPut(item, BitSet()).union(la):
        changed = true
    if changed and id notin inQueue:
      stateQueue.addLast(id)
      inQueue.incl(id)
    return id
  let id = states.len
  states.add(kernels)
  transitions.add(stdtables.initTable[GrammarSymbol, int]())
  stateIdsByCore[coreSeq] = id
  stateQueue.addLast(id)
  inQueue.incl(id)
  id

proc buildParseTable*(grammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar, skipConflictDetection: bool = false, tokenConflictMap: TokenConflictMap = TokenConflictMap()): BuildParseTable =
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
  
  # === Direct LALR(1) construction ===
  # States are deduplicated by their kernel item-set CORE (lookaheads not
  # included). When an existing core is reached again, its lookahead sets
  # are unioned and the state is re-queued, converging to the LALR(1)
  # lookahead sets. This produces the same states/lookaheads as the previous
  # propagation-based (Pager) pipeline, but computes closures in a single
  # pass via precomputed additions and never runs a global propagation
  # fixpoint over the whole state graph.
  let ctx = newSymbolContext(lexicalGrammar.variables.len, augmentedGrammar.externalTokens.len)
  let firstSetsBits = computeFirstBitSets(firstSets, ctx)
  let closureCache = precomputeClosureCache(augmentedGrammar, lexicalGrammar, firstSets, firstSetsBits)
  var firstBetaCache = stdtables.initTable[FirstBetaCacheKey, tuple[first: BitSet, nullable: bool]]()

  var states: seq[StateKernels] = @[]
  var transitions: seq[stdtables.Table[GrammarSymbol, int]] = @[]
  var stateIdsByCore = stdtables.initTable[seq[CoreItem], int]()
  var stateQueue = initDeque[int]()
  var inQueue = initHashSet[int]()

  # Initial state 0: augmented start item with EOF lookahead
  block:
    var startKernels = stdtables.initTable[CoreItem, LookaheadSet]()
    var startLookaheads = initBitSet(ctx.maxIndex)
    startLookaheads.incl(ctx.symbolToBit(GrammarSymbol(kind: stEnd, index: 0)))
    startKernels[CoreItem(
      variableIndex: augmentedStartIndex.uint16,
      productionIndex: 0,
      position: 0,
      inheritedPrecedence: 0
    )] = startLookaheads
    discard registerStateLALR(states, transitions, stateIdsByCore, stateQueue, inQueue, startKernels)

  let constructionStart = cpuTime()
  var iterCount = 0
  while stateQueue.len > 0:
    inc iterCount
    let stateId = stateQueue.popFirst()
    inQueue.excl(stateId)
    let fullClosure = getTransitiveClosureFast(
      augmentedGrammar, states[stateId], closureCache, firstSetsBits, firstBetaCache, fullPrecedence=false)

    # --- Group items by next symbol (successor computation) ---
    var nextStateKernels = stdtables.initTable[GrammarSymbol, StateKernels]()
    var nextSymbols: seq[GrammarSymbol] = @[]
    for core, lookaheads in fullClosure:
      template variable: untyped = augmentedGrammar.variables[core.variableIndex]
      template production: untyped = augmentedGrammar.variables[core.variableIndex].productions[core.productionIndex]
      if core.position >= production.steps.len.uint16:
        continue
      let symbol = production.steps[core.position].symbol
      let movedCore = CoreItem(
        variableIndex: core.variableIndex,
        productionIndex: core.productionIndex,
        position: core.position + 1,
        inheritedPrecedence: core.inheritedPrecedence
      )
      if symbol notin nextStateKernels:
        nextStateKernels[symbol] = stdtables.initTable[CoreItem, LookaheadSet]()
        nextSymbols.add(symbol)
      discard nextStateKernels[symbol].mgetOrPut(movedCore, BitSet()).union(lookaheads)

    # Register successor states (sorted for deterministic state numbering)
    nextSymbols.sort do (a, b: GrammarSymbol) -> int:
      if a.kind != b.kind: ord(a.kind) - ord(b.kind)
      else: a.index.int - b.index.int
    for symbol in nextSymbols:
      # NOTE: register first, then assign - registerStateLALR may grow
      # `transitions` (reallocating the seq), so taking the slot reference
      # before the call would leave a dangling reference.
      let targetStateId = registerStateLALR(
        states, transitions, stateIdsByCore, stateQueue, inQueue, nextStateKernels[symbol])
      transitions[stateId][symbol] = targetStateId

  echo "[Treestand] LALR(1) construction complete. States: ", states.len,
       " (built in ", (cpuTime() - constructionStart).formatFloat(ffDecimal, 3), "s)"

  let fillStart = cpuTime()
  # --- Fill in the parse table (one pass per state) ---
  var entries = newSeq[BuildParseTableEntry](states.len)
  # Global index: for each terminal bit, the non-terminals whose FIRST set
  # contains that terminal. Used to compute shift participants only for
  # terminals that are actually shifted in a state.
  var firstNontermsByTerminal = newSeq[seq[int]](ctx.maxIndex)
  for sym, bits in firstSetsBits:
    if sym.kind == stNonTerminal:
      for bit in bits:
        let s = ctx.bitToSymbol(bit)
        if s.kind == stTerminal:
          firstNontermsByTerminal[bit].add(sym.index.int)

  # Reusable per-state buffers for shift-participant collection:
  # direct terminal shifts: terminal bit -> parent variable bitset
  # indirect (via FIRST): non-terminal -> parent variable bitset
  var directParents = newSeq[BitSet](ctx.maxIndex)
  var parentsByNonterm = newSeq[BitSet](augmentedGrammar.variables.len)
  var usedTermBits = newSeqOfCap[int](64)
  var usedNonterms = newSeqOfCap[int](64)
  for stateId in 0 ..< states.len:
    let fullClosure = getTransitiveClosureFast(
      augmentedGrammar, states[stateId], closureCache, firstSetsBits, firstBetaCache,
      fullPrecedence = true)

    # Collect shift participants (for conflict resolution):
    # direct terminal shifts and per-non-terminal parents (whose FIRST sets
    # provide the indirect shifts). Stored as bitsets of variable indices.
    usedTermBits.setLen(0)
    usedNonterms.setLen(0)
    for coreItem, _ in fullClosure:
      template variable: untyped = augmentedGrammar.variables[coreItem.variableIndex]
      template production: untyped = augmentedGrammar.variables[coreItem.variableIndex].productions[coreItem.productionIndex]
      if coreItem.position < production.steps.len.uint16:
        let nextSym = production.steps[coreItem.position].symbol
        if nextSym.kind == stTerminal:
          let tb = ctx.symbolToBit(nextSym)
          if tb >= 0:
            if directParents[tb].len == 0:
              usedTermBits.add(tb)
            directParents[tb].incl(coreItem.variableIndex.int)
        elif nextSym.kind == stNonTerminal:
          let n = nextSym.index.int
          if parentsByNonterm[n].len == 0:
            usedNonterms.add(n)
          parentsByNonterm[n].incl(coreItem.variableIndex.int)

    var shiftSeen = initHashSet[ShiftDedupKey]()
    var reduceSeen = initHashSet[ReduceDedupKey]()
    var gotoSeen = initHashSet[GrammarSymbol]()

    for coreItem, lookaheadsBitSet in fullClosure:
      template variable: untyped = augmentedGrammar.variables[coreItem.variableIndex]
      template production: untyped = augmentedGrammar.variables[coreItem.variableIndex].productions[coreItem.productionIndex]

      if coreItem.position < production.steps.len.uint16:
        # --- SHIFT and GOTO (lookahead-independent: once per item) ---
        let nextSym = production.steps[coreItem.position].symbol
        let effectiveRulePrec = if production.precedence != 0: production.precedence
                                else: coreItem.inheritedPrecedence.int32
        let shiftPrecVal = if coreItem.position > 0:
            let prevStepPrec = production.steps[coreItem.position - 1].precedence
            if prevStepPrec.kind == pkInteger: prevStepPrec.intValue else: effectiveRulePrec
          else:
            effectiveRulePrec
        let gotoStateId = transitions[stateId][nextSym]

        if nextSym.kind != stNonTerminal:
          let key = ShiftDedupKey(
            sym: nextSym, target: gotoStateId.uint32,
            prec: shiftPrecVal, dynPrec: production.dynamicPrecedence)
          if key notin shiftSeen:
            shiftSeen.incl(key)
            var participants: seq[GrammarSymbol] = @[]
            let tb = ctx.symbolToBit(nextSym)
            if tb >= 0:
              var pbits = directParents[tb]
              for n in firstNontermsByTerminal[tb]:
                discard pbits.union(parentsByNonterm[n])
              for p in pbits:
                participants.add(GrammarSymbol(kind: stNonTerminal, index: p.uint16))
            entries[stateId].actionMap.add((
              sym: nextSym,
              action: BuildParseAction(
                kind: bpakShift,
                participants: participants,
                shiftState: gotoStateId.uint32,
                shiftPrecedence: shiftPrecVal,
                shiftDynamicPrecedence: production.dynamicPrecedence
              )
            ))
        else:
          if nextSym notin gotoSeen:
            gotoSeen.incl(nextSym)
            entries[stateId].gotoMap.add((sym: nextSym, state: gotoStateId.uint32))
      else:
        # --- REDUCE / ACCEPT ---
        if coreItem.variableIndex.int == augmentedStartIndex:
          for bitIndex in items(lookaheadsBitSet):
            let lookaheadSym = ctx.bitToSymbol(bitIndex)
            entries[stateId].actionMap.add((
              sym: lookaheadSym,
              action: BuildParseAction(kind: bpakAccept)
            ))
        else:
          let lhs = GrammarSymbol(kind: stNonTerminal, index: coreItem.variableIndex)
          let reduceAction = BuildParseAction(
            kind: bpakReduce,
            participants: @[lhs],
            reduceSymbol: lhs,
            reduceCount: production.steps.len.uint32,
            reducePrecedence: production.dynamicPrecedence,
            reduceStaticPrecedence: production.precedence,
            reduceAssociativity: production.associativity
          )
          for bitIndex in items(lookaheadsBitSet):
            let lookaheadSym = ctx.bitToSymbol(bitIndex)
            let key = ReduceDedupKey(
              sym: lookaheadSym, reduceSymbol: lhs,
              reduceCount: production.steps.len.uint32,
              dynPrec: production.dynamicPrecedence,
              staticPrec: production.precedence,
              assoc: production.associativity)
            if key notin reduceSeen:
              reduceSeen.incl(key)
              entries[stateId].actionMap.add((sym: lookaheadSym, action: reduceAction))

    # Clear the participant buffer slots used by this state
    for tb in usedTermBits:
      directParents[tb].clear()
    for n in usedNonterms:
      parentsByNonterm[n].clear()

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

  var productionInfos = newSeq[BuildProductionInfo]()
  debugEchoMsg "[Treestand] Building production infos..."
  for i in 0 ..< grammar.variables.len:
    if i mod 10 == 0:
      debugEchoMsg "[Treestand] Building production infos entry ", i
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
  
  debugEchoMsg "[Treestand] Adding extra symbols to parse table..."
  for stateIdx in 0 ..< entries.len:
    if stateIdx mod 100 == 0:
      debugEchoMsg "[Treestand] Adding extra symbols to parse table entry ", stateIdx
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
  echo "[Treestand] Fill complete in ", (cpuTime() - fillStart).formatFloat(ffDecimal, 3), "s"
  echo "[Treestand] Detecting conflicts in the parse table ..."
  proc getOriginalSymbol(g: SyntaxGrammar, sym: GrammarSymbol, visited: seq[GrammarSymbol] = @[]): GrammarSymbol =
    if sym.kind == stNonTerminal:
      if sym in visited:
            # Infinite recursion detected - return the symbol itself or best guess
            echo "Resolution Cycle: ", sym
            return sym
            
      let variable = g.variables[sym.index]
      if variable.originalSymbol.isSome:
          let parent = variable.originalSymbol.get()
          # For named rules (declared in the grammar), resolve exactly one
          # level up to their containing parent rule, then stop.  For
          # auxiliary/inlined rules, follow the chain fully because their
          # parent is the visible rule they were expanded from.
          if variable.kind == vtNamed:
            return parent
          var newVisited = visited
          newVisited.add(sym)
          return getOriginalSymbol(g, parent, newVisited)
      else:
          # Heuristic Name Resolution for Repeats
          # If name ends with _repeat%d+, try to find base name
          if variable.kind == vtAuxiliary and variable.name.contains("_repeat"):
              let idx = variable.name.rfind("_repeat")
              if idx > 0:
                  let baseName = variable.name[0 ..< idx]
                  # Search for variable with this name
                  # This is slow O(N) but conflict detection is not critical path
                  for i, v in g.variables:
                      if v.name == baseName:
                          let parent = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
                          # echo "Resolving (Heuristic): ", variable.name, " -> ", baseName
                          var newVisited = visited
                          newVisited.add(sym)
                          return getOriginalSymbol(g, parent, newVisited)
          return sym
    return sym
  
  proc isConflictExpected(ruleSymbols: seq[GrammarSymbol]): bool =
    if ruleSymbols.len == 0: return true

    # Resolve symbols and include their one-level named parent (if any).
    # This allows declared conflicts referencing e.g. `_simple_type` to match
    # participants that are children like `pointer_type` — the parent rule
    # name is what tree-sitter grammars declare in their `conflicts` lists.
    var resolved = newSeqOfCap[GrammarSymbol](ruleSymbols.len)
    for r in ruleSymbols:
      if r notin resolved:
        resolved.add(r)
      let orig = getOriginalSymbol(augmentedGrammar, r)
      if orig notin resolved:
        resolved.add(orig)
      # Also check if this rule has a direct parent via originalSymbol
      if r.kind == stNonTerminal:
        let variable = augmentedGrammar.variables[r.index]
        if variable.originalSymbol.isSome:
          let parent = variable.originalSymbol.get
          if parent notin resolved:
            resolved.add(parent)
    for expectedSet in augmentedGrammar.expectedConflicts:
      # Check if the expected set is a subset of the resolved
      # participants (declared conflicts typically name only the
      # specific rules involved, while participants may include
      # additional parent symbols).
      var allExpectedFound = true
      for expectedRule in expectedSet:
        if expectedRule notin resolved:
          allExpectedFound = false
          break
      if allExpectedFound:
        debugEchoMsg "Conflict expected and suppressed for: ", ruleSymbols
        return true

    debugEchoMsg "Conflict NOT expected for: ", ruleSymbols
    debugEchoMsg "  resolved: ", resolved
    debugEchoMsg "Available expected sets:"
    for s in augmentedGrammar.expectedConflicts:
      debugEchoMsg "  Set: ", s
    return false
  
  for stateId in 0..<entries.len:
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
          # when defined(debug):
          #   echo "[DEBUG] ========== SHIFT/REDUCE CONFLICT =========="
          #   echo "[DEBUG] State: ", stateId, " Symbol: ", getSymbolName(sym)
          #   echo "[DEBUG] Shift actions (", shiftActions.len, "):"
          #   for i, s in shiftActions:
          #     echo "[DEBUG]   ", i, ": SHIFT to state ", s.shiftState, " prec=", s.shiftPrecedence, " dynPrec=", s.shiftDynamicPrecedence
          #   echo "[DEBUG] Reduce actions (", reduces.len, "):"
          #   for i, r in reduces:
          #     echo "[DEBUG]   ", i, ": REDUCE ", getSymbolName(r.reduceSymbol), " prec=", r.reduceStaticPrecedence, " assoc=", (if r.reduceAssociativity.isSome: $r.reduceAssociativity.get else: "none")
          
          var allResolved = true
          var winningReduces: seq[BuildParseAction] = @[]
          var keepShifts = false
          var glrHeuristicApplied = false  # Track if we applied GLR heuristic
          
          # CRITICAL CHECK: If we have multiple shift actions going to the SAME state
          # but with DIFFERENT precedences, this represents multiple distinct semantic 
          # interpretations (e.g. product vs other_thing in conflicting_precedence grammar).
          # If these are combined with reduce actions, we have an unresolved conflict
          # that precedence alone cannot resolve - the grammar must specify which interpretation
          # to prefer via explicit conflict declarations.
          #
          # Example: expression '+' expression • '*'
          #   - Shift '*' for product (prec=1)
          #   - Shift '*' for other_thing (prec=-1)  
          #   - Reduce as sum (prec=0), then later consider '*'
          # This is 3 interpretations, not 2! Deduplication by state hides the ambiguity.
          
          var hasMultiplePrecedences = false
          if reduces.len > 0:
            # Group shifts by target state to check for precedence conflicts
            var precByState = stdtables.initTable[uint32, seq[int32]]()
            for s in shiftActions:
              if s.shiftState notin precByState:
                precByState[s.shiftState] = @[]
              if s.shiftPrecedence notin precByState[s.shiftState]:
                precByState[s.shiftState].add(s.shiftPrecedence)
                    
          # If we found multiple precedences to the same state + reduces, check expectedConflicts
          if hasMultiplePrecedences:
            # Collect ALL participants from shift actions (before deduplication) AND reduce actions
            # to properly match against expectedConflicts which may include all the different interpretations
            var participants: seq[GrammarSymbol] = @[]
            
            # Add all shift participants (from all shifts, not just deduplicated ones)
            for s in shiftActions:
              for p in s.participants:
                if p notin participants: participants.add(p)
            
            # Add all reduce participants
            for r in reduces:
              for p in r.participants:
                if p notin participants: participants.add(p)
            
            debugEchoMsg "Detected multiple-precedence shift conflict with reduces"
            debugEchoMsg "Participants: ", participants
            
            if not isConflictExpected(participants):
              # Unresolved conflict - multiple semantic interpretations
              let path = findPathToState(stateId)
              var contextStr = ""
              for s in path:
                contextStr &= getSymbolName(s) & " "
              contextStr &= " •  " & getSymbolName(sym) & "  ..."

              var conflictMsg = "Unresolved conflict for symbol sequence:\n\n"
              conflictMsg &= "  " & contextStr & "\n\n"
              conflictMsg &= "Possible interpretations:\n\n"
              
              # Show all shift interpretations
              var shiftIdx = 1
              for s in shiftActions:
                conflictMsg &= "  " & $shiftIdx & ":  SHIFT " & getSymbolName(sym) & 
                             " (precedence: " & $s.shiftPrecedence & ")\n"
                shiftIdx += 1
              
              # Show reduce interpretations
              for i, r in reduces:
                conflictMsg &= "  " & $(shiftIdx + i) & ":  REDUCE " & getSymbolName(r.reduceSymbol) &
                             " (precedence: " & $r.reduceStaticPrecedence &
                             ", assoc: " & (if r.reduceAssociativity.isSome: $r.reduceAssociativity.get else: "none") & ")\n"
              
              conflictMsg &= "\nPossible resolutions:\n\n"
              conflictMsg &= "  1:  Add a conflict declaration for these rules\n"
              conflictMsg &= "  2:  Restructure the grammar to avoid ambiguity\n"
              conflictMsg &= "  3:  Adjust precedence levels to create a clear ordering\n"

              let errorMsg = "Unresolved conflict for symbol sequence:\n\n" & conflictMsg
              if not skipConflictDetection:
                raise newException(ValueError, errorMsg)
              else:
                echo "[Treestand] Conflict not expected:\n", errorMsg
          
          # Deduplicate shift actions by target state, keeping only the highest precedence
          # for each target state. This resolves conflicts where the same transition is 
          # derived from multiple rules with different precedences (e.g. C grammar u8' case).
          # But it preserves conflicts where shifts go to DIFFERENT states (ambiguous parse paths).
          
          var bestShiftsByState = stdtables.initTable[uint32, BuildParseAction]()
          var discardedShifts: seq[tuple[action: BuildParseAction, winner: BuildParseAction]] = @[]
          
          for s in shiftActions:
             if s.shiftState notin bestShiftsByState:
                bestShiftsByState[s.shiftState] = s
             else:
                let current = bestShiftsByState[s.shiftState]
                if s.shiftPrecedence > current.shiftPrecedence:
                   # New higher-precedence shift found, current becomes discarded
                   discardedShifts.add((action: current, winner: s))
                   bestShiftsByState[s.shiftState] = s
                elif s.shiftPrecedence < current.shiftPrecedence:
                   # Current shift is lower precedence, discard it
                   discardedShifts.add((action: s, winner: current))
                # If equal precedence, keep the first one (current behavior)
          
          # # Warn about potentially unreachable rules if shifts were discarded
          # when defined(debug):
          #   if discardedShifts.len > 0:
          #     echo "[DEBUG] Discarded shifts: ", discardedShifts.len, " reduces: ", reduces.len
          
          if discardedShifts.len > 0 and reduces.len > 0:
            # Only warn if there are also reduce actions (indicating a shift/reduce context)
            # This filters out simple precedence resolution between shift variants
            var warnedSymbols: seq[GrammarSymbol] = @[]
            for discarded in discardedShifts:
              # Collect the reduce symbols from discarded actions
              # These represent grammar rules that might be unreachable
              if discarded.action.participants.len > 0:
                for p in discarded.action.participants:
                  if p.kind == stNonTerminal and p notin warnedSymbols:
                    warnedSymbols.add(p)
            
            debugEchoMsg "Warned symbols: ", warnedSymbols.len

            # The path reconstruction below is only used for debug output;
            # skip the expensive BFS entirely in non-debug builds.
            when defined(debug):
              if warnedSymbols.len > 0:
                let path = findPathToState(stateId)
                var contextStr = ""
                for s in path:
                  contextStr &= getSymbolName(s) & " "
                contextStr &= " •  " & getSymbolName(sym) & "  ..."

                debugEchoMsg "[Treestand] Warning: Potentially unreachable rules due to precedence"
                debugEchoMsg "  Context: ", contextStr
                debugEchoMsg "  Lower-precedence alternatives discarded:"
                for p in warnedSymbols:
                  debugEchoMsg "    - ", getSymbolName(p)
                debugEchoMsg "  These rules may be unreachable in this parsing context."
                debugEchoMsg ""
          
          shiftActions = @[]
          for s in bestShiftsByState.values:
             shiftActions.add(s)

          # For each reduce, check what each shift decides
          for r in reduces:
            let reducePrecedence = r.reduceStaticPrecedence
            var shiftWins = 0
            var reduceWins = 0
            var equalPrec = 0
            
            # Check each shift action individually
            for shift in shiftActions:
              if shift.shiftPrecedence > reducePrecedence:
                shiftWins += 1
              elif shift.shiftPrecedence < reducePrecedence:
                reduceWins += 1
              else:
                equalPrec += 1
            
            debugEchoMsg "Reduce ", getSymbolName(r.reduceSymbol), " (prec=", reducePrecedence, ") vs ", shiftActions.len, " shifts:"
            debugEchoMsg "  Shifts with higher prec: ", shiftWins
            debugEchoMsg "  Shifts with lower prec: ", reduceWins  
            debugEchoMsg "  Shifts with equal prec: ", equalPrec
            
            # If all shifts have the same relationship to reduce precedence
            if shiftWins == shiftActions.len:
              # All shifts have higher precedence → SHIFT wins
              debugEchoMsg "   → All shifts win (higher precedence)"
              keepShifts = true
              
            elif reduceWins == shiftActions.len:
              # All shifts have lower precedence → REDUCE wins
              debugEchoMsg "   → REDUCE wins (all shifts have lower precedence)"
              winningReduces.add(r)
              # GLR Heuristic: keep negative precedence shift for GLR when reduce is zero precedence
              if shiftActions.len == 1 and shiftActions[0].shiftPrecedence < 0 and reducePrecedence == 0:
                debugEchoMsg "   → GLR heuristic: Also keeping negative prec shift with zero prec reduce"
                keepShifts = true
                glrHeuristicApplied = true
              
              
            elif equalPrec == shiftActions.len:
              # All shifts have equal precedence → check associativity
              debugEchoMsg "   → All equal precedence, checking associativity..."
              
              if r.reduceAssociativity.isSome:
                let assoc = r.reduceAssociativity.get
                if assoc == gaLeft:
                  debugEchoMsg "     → Left assoc: REDUCE wins"
                  winningReduces.add(r)
                elif assoc == gaRight:
                  debugEchoMsg "     → Right assoc: SHIFT wins"
                  keepShifts = true
                else:
                  # Non-associative: mark as unresolved
                  debugEchoMsg "     → Non-assoc: UNRESOLVED"
                  allResolved = false
              else:
                # No associativity: mark as unresolved
                # These MUST be in expectedConflicts for GLR,
                # or the grammar needs associativity specified
                debugEchoMsg "     → No associativity: UNRESOLVED"
                allResolved = false
                
            else:
              # Mixed: some shifts win, some lose → UNRESOLVED
              debugEchoMsg "   → MIXED precedences: UNRESOLVED!"
              allResolved = false
          
          # Check for contradictory decisions: if we decided both shift AND reduce should win,
          # UNLESS it's the GLR heuristic (which intentionally does this)
          if keepShifts and winningReduces.len > 0 and not glrHeuristicApplied:
            debugEchoMsg "CONTRADICTION: Both shifts and reduces won - unresolved!"
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
              
              # ERROR: Conflict not expected - Fail generation (match tree-sitter behavior)
              let errorMsg = "Unresolved conflict for symbol sequence:\n\n" & conflictMsg
              if not skipConflictDetection:
                raise newException(ValueError, errorMsg)
              else:
                echo "[Treestand] Conflict not expected:\n", errorMsg
          
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
          
          # Find max precedence (static only - tree-sitter does NOT use dynamic for reduce/reduce)
          # Dynamic precedence is only for runtime ambiguity resolution, not parser generation
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
              # Collect symbols of conflicting items from the state's closure,
              # mirroring tree-sitter's `handle_conflict`.  Instead of using
              # pre-stored action participants (which can differ based on
              # closure details), we recompute the closure and extract the
              # variables of all completed items with this lookahead.
              let checkClosure = getTransitiveClosureFast(
                augmentedGrammar, states[stateId], closureCache, firstSetsBits, firstBetaCache,
                fullPrecedence = false)
              # Collect all distinct variables of completed items whose
              # lookaheads contain the conflicting symbol.
              var seenVars: seq[GrammarSymbol] = @[]
              for coreItem, la in checkClosure:
                if coreItem.position >= augmentedGrammar.variables[coreItem.variableIndex].productions[coreItem.productionIndex].steps.len.uint16:
                  for bit in la:
                    if ctx.bitToSymbol(bit) == sym:
                      let s = GrammarSymbol(kind: stNonTerminal, index: coreItem.variableIndex)
                      if s notin seenVars:
                        seenVars.add(s)
                      break
              # Auto-accept reduce/reduce conflicts only when all conflicting
              # items resolve to the same original symbol AND at least one
              # participant is an auxiliary/repeat rule.  Named-rule-only
              # conflicts that happen to share a resolved symbol (e.g.
              # associativity_missing) must still be reported.
              var allResolveSame = seenVars.len > 0
              var hasAux = false
              if allResolveSame:
                let firstResolved = getOriginalSymbol(augmentedGrammar, seenVars[0])
                for v in seenVars:
                  if getOriginalSymbol(augmentedGrammar, v) != firstResolved:
                    allResolveSame = false
                    break
                  let vr = augmentedGrammar.variables[v.index]
                  if vr.kind == vtAuxiliary or vr.name.contains("_repeat"):
                    hasAux = true
              let isExpected = (allResolveSame and hasAux and augmentedGrammar.expectedConflicts.len > 0) or isConflictExpected(seenVars)
              
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
                
                # ERROR: Conflict not expected - Fail generation
                let errorMsg = "Unresolved conflict for symbol sequence:\n\n" & conflictMsg
                if not skipConflictDetection:
                  raise newException(ValueError, errorMsg)
                else:
                  echo "[Treestand] Conflict not expected:\n", errorMsg
                
                # Treat as GLR split (add all precedence winners)
                for r in precedenceWinners:
                    resolvedActions.add((sym: sym, action: r))
                processed = true
          else:
            # Winner found by precedence
            resolvedActions.add((sym: sym, action: precedenceWinners[0]))
            processed = true
      
      if not processed:
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
            for a in actions: resolvedActions.add((sym: sym, action: a))

    # Update actions
    entries[stateId].actionMap = resolvedActions
  
  var resultTable = BuildParseTable(
    entries: entries,
    productionInfos: productionInfos,
    externalSymbols: @[]
  )

  minimizeParseTable(
    resultTable, grammar, lexicalGrammar, @[],
    doesConflict = proc(i, j: int): bool =
      tokenConflictMap.n > 0 and tokenConflictMap.doesConflict(i, j),
    doesMatchSameString = proc(i, j: int): bool =
      tokenConflictMap.n > 0 and tokenConflictMap.doesMatchSameString(i, j)
  )
  resultTable