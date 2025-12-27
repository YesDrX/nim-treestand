## Grammar data structures representing different stages of the generation pipeline.

import nfa
import std/tables as stdtables
import std/[hashes, options]

type
  VariableType* = enum
    ## Classifies the role of a grammar variable (Rule).
    vtHidden     ## Internal rule, not exposed in the CST (prefixed with `_`).
    vtAuxiliary  ## Generated helper rule (e.g. for repetition or sequences).
    vtAnonymous  ## String literal or pattern (e.g. "if", "\d+").
    vtNamed      ## Standard named rule defined in the grammar.

  PrecedenceEntry* = object
    ## Helper structure for defining precedence orderings explicitly.
    case isName*: bool
    of true:
      name*: string
    of false:
      symbol*: string

  ReservedWordContext*[T] = object
    ## Defines a scope where certain words are reserved.
    name*: string
    reservedWords*: seq[T]

  Variable* = object
    ## Represents a high-level rule definition from the user's input grammar.
    name*: string
    kind*: VariableType
    rule*: Rule

  InputGrammar* = object
    ## The raw grammar definition provided by the user (or DSL).
    ## This is the input to the `prepareGrammar` phase.
    name*: string                                ## Examples: "json", "python".
    variables*: seq[Variable]                    ## List of all defined rules.
    extraSymbols*: seq[Rule]                     ## Whitespace, comments to be skipped.
    expectedConflicts*: seq[seq[string]]         ## Documented LR conflicts to suppress warnings.
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    externalTokens*: seq[Rule]                   ## Tokens handled by external scanner.
    variablesToInline*: seq[string]              ## Rules to inline into their callers.
    supertypeSymbols*: seq[string]               ## Rules grouped as supertypes (for query API).
    wordToken*: Option[string]                   ## Keyword tokenizer optimization.
    reservedWords*: seq[ReservedWordContext[Rule]]
    externalScanner*: string                     ## Path to external scanner (scanner.nim or scanner.c).

  LexicalVariable* = object
    ## A refined variable for the Lexical Grammar (Lexer).
    ## These are typically Terminals (tokens).
    name*: string
    kind*: VariableType
    implicitPrecedence*: int32
    startState*: uint32          ## Starting state in the Lexer NFA/DFA.
    rule*: Rule

  LexicalGrammar* = object
    ## The portion of the grammar handled by the Lexer.
    ## Consists of Regex/String rules converted to NFA.
    nfa*: Nfa
    variables*: seq[LexicalVariable]

  ReservedWordSetId* = distinct uint

  ProductionStep* = object
    ## A single element in a production sequence (RHS).
    symbol*: GrammarSymbol
    precedence*: Precedence
    associativity*: Option[GrammarAssociativity]
    alias*: Option[Alias]
    fieldName*: Option[string]
    reservedWordSetId*: ReservedWordSetId

  Production* = object
    ## A single alternative in a Syntax Variable (e.g. one branch of a Choice).
    steps*: seq[ProductionStep]
    dynamicPrecedence*: int32
    precedence*: int32  # Static precedence
    associativity*: Option[GrammarAssociativity]

  InlinedProductionMap* = object
    productions*: seq[Production]
    productionMap*: stdtables.Table[(pointer, uint32), seq[int]]

  SyntaxVariable* = object
    ## A variable in the Syntax Grammar (Parser).
    ## Typically Non-Terminals defined by sequences of ProductionSteps.
    name*: string
    kind*: VariableType
    productions*: seq[Production]
    isNullable*: bool

  ExternalToken* = object
    name*: string
    kind*: VariableType
    correspondingInternalToken*: Option[GrammarSymbol]

  SyntaxGrammar* = object
    ## The processed grammar ready for Parse Table generation.
    ## Variables here are Non-Terminals composed of other Symbols (Terminals/Non-Terminals).
    variables*: seq[SyntaxVariable]
    extraSymbols*: seq[GrammarSymbol]
    expectedConflicts*: seq[seq[GrammarSymbol]]
    externalTokens*: seq[ExternalToken]
    variablesToInline*: seq[GrammarSymbol]
    supertypeSymbols*: seq[GrammarSymbol]
    wordToken*: Option[GrammarSymbol]
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    reservedWordSets*: seq[TokenSet]

  GrammarSymbolType* {.size: 2.} = enum
    ## Distinguishes between different kinds of symbols in the grammar.
    stExternal              ## Symbol managed by an external scanner (C/C++).
    stEnd                   ## End of input (EOF) or end of sequence.
    stEndOfNonTerminalExtra ## Special internal symbol for GLR processing.
    stTerminal              ## A lexical token (leaf node e.g., string literal, regex).
    stNonTerminal           ## A syntactic rule composed of other symbols.

  GrammarAssociativity* = enum
    ## Specifies associativity for binary operations.
    gaLeft   ## Left-associative: a + b + c parsed as (a + b) + c
    gaRight  ## Right-associative: a = b = c parsed as a = (b = c)

  Alias* = object
    ## Represents a renamed symbol in the parse tree (e.g., mapping "unary_minus" to "-").
    value*: string  ## The alias name.
    isNamed*: bool  ## Whether the alias appears as a named node in the CST.

  PrecedenceKind* = enum
    pkNone     ## No precedence specified.
    pkInteger  ## Numeric precedence level (higher binds tighter).
    pkName     ## Named precedence (unresolved, used during grammar prep).

  Precedence* = object
    ## Encapsulates precedence information for a rule or production step.
    case kind*: PrecedenceKind
    of pkNone:
      discard
    of pkInteger:
      intValue*: int32  ## The numeric precedence value.
    of pkName:
      name*: string     ## The unresolved precedence name.

  MetadataParams* = object
    ## Stores metadata associated with a rule, affecting parsing behavior.
    precedence*: Precedence              ## Static precedence.
    dynamicPrecedence*: int32            ## Dynamic precedence (runtime conflict resolution).
    associativity*: Option[GrammarAssociativity] ## Left or Right associativity.
    isToken*: bool                       ## Marks a rule as a lexical token (atomic).
    isMainToken*: bool                   ## Internal flag for main lexer tokens.
    alias*: Option[Alias]                ## Alias for the resulting node.
    fieldName*: Option[string]           ## Field name for structure access (e.g. `left:`).

  GrammarSymbol* = object
    ## Represents a unique symbol in the grammar, identified by type and index.
    ## This is the build-time representation, distinct from `parser_types.Symbol`.
    kind*: GrammarSymbolType
    index*: uint16  ## Index into the respective symbol list (terminals, non-terminals, etc.).

  RuleKind* = enum
    ## Enumeration of all possible AST node types for the grammar DSL.
    rkBlank        ## Empty rule.
    rkString       ## Literal string match (e.g. "if").
    rkPattern      ## Regex pattern (e.g. "\d+").
    rkNamedSymbol  ## Reference to another named rule (variable).
    rkSymbol       ## Resolved symbol reference (internal use).
    rkChoice       ## Alternative options (OR / |).
    rkMetadata     ## Wraps another rule with metadata (prec, assoc, token).
    rkRepeat       ## Repetition (zero-or-more, one-or-more).
    rkSeq          ## Sequence of rules (concatenation).
    rkReserved     ## Reserved word handling.

  Rule* = object
    ## The core recursive data structure representing a grammar rule expression.
    ## This AST is built via the DSL and processed to generate the parser.
    case kind*: RuleKind
    of rkBlank:
      discard
    of rkString:
      stringValue*: string
    of rkPattern:
      patternValue*: string
      patternFlags*: string
    of rkNamedSymbol:
      symbolName*: string
    of rkSymbol:
      symbol*: GrammarSymbol
    of rkChoice:
      choiceMembers*: seq[Rule]
    of rkMetadata:
      metadataParams*: MetadataParams
      metadataRule*: ref Rule
    of rkRepeat:
      repeatContent*: ref Rule
    of rkSeq:
      seqMembers*: seq[Rule]
    of rkReserved:
      reservedRule*: ref Rule
      reservedContextName*: string

  TokenSet* = object
    ## Bitset-like structure to track relevant tokens for Lexer state generation.
    terminalBits*: seq[bool]
    externalBits*: seq[bool]
    eof*: bool
    endOfNonTerminalExtra*: bool

  AliasMap* = Table[GrammarSymbol, Alias]

  BuildParseActionKind* = enum
    bpakShift
    bpakReduce
    bpakAccept
    bpakError
    bpakShiftExtra  # For external tokens in extras - consume but don't push to stack
    bpakSplit

  BuildParseAction* = object
    participants*: seq[GrammarSymbol] # Rules that derived this action
    case kind*: BuildParseActionKind
    of bpakShift:
      shiftState*: uint32
      shiftPrecedence*: int32
      shiftDynamicPrecedence*: int32
    of bpakReduce:
      reduceSymbol*: GrammarSymbol
      reduceCount*: uint32
      reducePrecedence*: int32
      reduceStaticPrecedence*: int32
      reduceAssociativity*: Option[GrammarAssociativity]
    of bpakAccept, bpakError, bpakShiftExtra:
      discard
    of bpakSplit:
      splitActions*: seq[BuildParseAction]

  BuildParseTableEntry* = object
    # Map from symbol to action for O(1) lookup
    actionMap*: seq[tuple[sym: GrammarSymbol, action: BuildParseAction]]
    # GOTO table for non-terminals (symbol -> next state)
    gotoMap*: seq[tuple[sym: GrammarSymbol, state: uint32]]
    # Index of the lexical state to use for this parser state
    lexState*: uint32

  BuildProductionInfo* = object
    symbol*: GrammarSymbol
    fieldCount*: uint32
    childCount*: uint32
    # Field names for this production
    fieldNames*: seq[string]

  BuildParseTable* = object
    entries*: seq[BuildParseTableEntry]
    productionInfos*: seq[BuildProductionInfo]
    # External symbol list
    externalSymbols*: seq[GrammarSymbol]

  BuildLexState* = object
    transitions*: seq[BuildLexTransition]
    accept*: Option[int] # Index of accepted variable, if any

  BuildLexTransition* = object
    characters*: CharacterSet
    isSeparator*: bool
    precedence*: int32
    state*: uint32

  BuildLexTable* = object
    states*: seq[BuildLexState]

const
  NoReservedWords* = ReservedWordSetId(high(uint))

# --------------- Precedence ---------------

proc newPrecedence*(value: int32): Precedence =
  Precedence(kind: pkInteger, intValue: value)

proc newPrecedence*(name: string): Precedence =
  Precedence(kind: pkName, name: name)

proc nonePrecedence*(): Precedence =
  Precedence(kind: pkNone)

proc `==`*(a, b: Precedence): bool =
  if a.kind != b.kind: return false
  case a.kind
  of pkNone: true
  of pkInteger: a.intValue == b.intValue
  of pkName: a.name == b.name

# --------------- Grammar Symbol ---------------

proc newGrammarSymbol*(kind: GrammarSymbolType, index: int): GrammarSymbol =
  GrammarSymbol(kind: kind, index: index.uint16)

proc `==`*(a, b: GrammarSymbol): bool =
  a.kind == b.kind and a.index == b.index

proc hash*(s: GrammarSymbol): Hash =
  hash((s.kind, s.index))

proc `$`*(s: GrammarSymbol): string =
  case s.kind
  of stExternal: "external(" & $s.index & ")"
  of stEnd: "end"
  of stEndOfNonTerminalExtra: "end_of_nonterminal_extra"
  of stTerminal: "terminal(" & $s.index & ")"
  of stNonTerminal: "non_terminal(" & $s.index & ")"

# --------------- Rule ---------------

proc hash*(r: Rule): Hash =
  ## Structural hash for Rule
  var h: Hash = 0
  h = h !& hash(r.kind)
  case r.kind
  of rkBlank: discard
  of rkString: h = h !& hash(r.stringValue)
  of rkPattern:
    h = h !& hash(r.patternValue)
    h = h !& hash(r.patternFlags)
  of rkNamedSymbol: h = h !& hash(r.symbolName)
  of rkSymbol: h = h !& hash(r.symbol)
  of rkChoice:
    for m in r.choiceMembers:
      h = h !& hash(m)
  of rkMetadata:
    # Hash metadata params (simplified)
    h = h !& hash(r.metadataParams.dynamicPrecedence)
    h = h !& hash(r.metadataParams.isToken)
    h = h !& hash(r.metadataRule[])
  of rkRepeat:
    h = h !& hash(r.repeatContent[])
  of rkSeq:
    for m in r.seqMembers:
      h = h !& hash(m)
  of rkReserved:
    h = h !& hash(r.reservedRule[])
    h = h !& hash(r.reservedContextName)
  !$h

proc `==`*(a, b: Rule): bool =
  if a.kind != b.kind: return false
  case a.kind
  of rkBlank: true
  of rkString: a.stringValue == b.stringValue
  of rkPattern: a.patternValue == b.patternValue and a.patternFlags == b.patternFlags
  of rkNamedSymbol: a.symbolName == b.symbolName
  of rkSymbol: a.symbol == b.symbol
  of rkChoice:
    if a.choiceMembers.len != b.choiceMembers.len: return false
    for i in 0 ..< a.choiceMembers.len:
      if a.choiceMembers[i] != b.choiceMembers[i]: return false
    true
  of rkMetadata:
    a.metadataParams == b.metadataParams and a.metadataRule[] == b.metadataRule[]
  of rkRepeat: a.repeatContent[] == b.repeatContent[]
  of rkSeq:
    if a.seqMembers.len != b.seqMembers.len: return false
    for i in 0 ..< a.seqMembers.len:
      if a.seqMembers[i] != b.seqMembers[i]: return false
    true
  of rkReserved:
    a.reservedContextName == b.reservedContextName and a.reservedRule[] == b.reservedRule[]

# --------------- Build Parse Action ---------------

proc `==`*(a, b: BuildParseAction): bool {.noSideEffect.} =
  if a.kind != b.kind: return false
  if a.participants != b.participants: return false
  case a.kind
  of bpakShift:
    a.shiftState == b.shiftState and 
    a.shiftPrecedence == b.shiftPrecedence and
    a.shiftDynamicPrecedence == b.shiftDynamicPrecedence
  of bpakReduce:
    a.reduceSymbol == b.reduceSymbol and 
    a.reduceCount == b.reduceCount and 
    a.reducePrecedence == b.reducePrecedence and 
    a.reduceStaticPrecedence == b.reduceStaticPrecedence and
    a.reduceAssociativity == b.reduceAssociativity
  of bpakAccept, bpakError, bpakShiftExtra:
    true
  of bpakSplit:
    if a.splitActions.len != b.splitActions.len: return false
    for i in 0 ..< a.splitActions.len:
      if a.splitActions[i] != b.splitActions[i]: return false
    true

proc hash*(a: BuildParseAction): Hash =
  var h: Hash = 0
  h = h !& hash(a.kind)
  h = h !& hash(a.participants)
  case a.kind
  of bpakShift:
    h = h !& hash(a.shiftState)
    h = h !& hash(a.shiftPrecedence)
    h = h !& hash(a.shiftDynamicPrecedence)
  of bpakReduce:
    h = h !& hash(a.reduceSymbol)
    h = h !& hash(a.reduceCount)
    h = h !& hash(a.reducePrecedence)
    h = h !& hash(a.reduceStaticPrecedence)
    h = h !& hash(a.reduceAssociativity)
  of bpakAccept, bpakError, bpakShiftExtra:
    discard
  of bpakSplit:
    h = h !& hash(a.splitActions)
  !$h
