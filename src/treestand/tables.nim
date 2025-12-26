## Parse table data structures

import rules, nfa, options, std/hashes

type
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

type
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

