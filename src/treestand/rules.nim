## Rule data structures and operations

import std/tables, hashes, options, math

type
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

proc newSymbol*(kind: GrammarSymbolType, index: int): GrammarSymbol =
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
