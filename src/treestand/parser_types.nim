##
##  This file is imported in generated parsers to provide common type definitions
## 
##  * When a parser is generated, it will import this file `import treestand/parser_types`
##

{.push warning[UnusedImport]: off.}
import std/[options, times, strformat, hashes]
{.pop.}

type  
  # Compact Table Types
  ActionEntry* = tuple
    sym: int16
    kind: int8
    data: uint32

  GotoEntry* = tuple
    sym: int16
    state: uint32

  TableIndex* = tuple
    actionStart: int32
    actionLen: int32
    gotoStart: int32
    gotoLen: int32
    lexState: int32

  SymbolKind* = enum
    skTerminal
    skNonTerminal

  Symbol* = object
    case kind*: SymbolKind
    of skTerminal:
      terminalIndex*: int
    of skNonTerminal:
      nonTerminalIndex*: int

proc terminal*(idx: int): Symbol =
  Symbol(kind: skTerminal, terminalIndex: idx)

proc nonTerminal*(idx: int): Symbol =
  Symbol(kind: skNonTerminal, nonTerminalIndex: idx)

proc `==`*(a, b: Symbol): bool =
  if a.kind != b.kind: return false
  case a.kind
  of skTerminal: a.terminalIndex == b.terminalIndex
  of skNonTerminal: a.nonTerminalIndex == b.nonTerminalIndex

# Symbol metadata - distinguishes named nodes from anonymous tokens
type
  SymbolMetadata* = object
    named*: bool  ## True for named nodes like 'function_declaration'
                  ## False for anonymous tokens like '+' or 'if'

# Parse table data
type
  Associativity* = enum
    assocNone
    assocLeft
    assocRight

  ParseActionKind* = enum
    pakShift
    pakReduce
    pakAccept
    pakError
    pakShiftExtra
    pakSplit

  ParseAction* = object
    case kind*: ParseActionKind
    of pakShift:
      shiftState*: uint32
    of pakReduce:
      reduceSymbol*: Symbol
      reduceCount*: uint32
      reducePrecedence*: int32
      reduceStaticPrecedence*: int32
      reduceAssociativity*: Option[Associativity]
      reduceDynamicPrecedence*: int32
    of pakAccept, pakError, pakShiftExtra:
      discard
    of pakSplit:
      splitActions*: seq[ParseAction]

  ParseTableEntry* = object
    actionMap*: seq[tuple[sym: Symbol, action: ParseAction]]
    gotoMap*: seq[tuple[sym: Symbol, state: uint32]]
    lexState*: int

proc findAction*(entry: ParseTableEntry, lookahead: Symbol): ParseAction =
  for (sym, action) in entry.actionMap:
    if sym == lookahead:
      return action
  return ParseAction(kind: pakError)

proc findGoto*(entry: ParseTableEntry, symbol: Symbol): int =
  for (sym, state) in entry.gotoMap:
    if sym == symbol:
      return state.int
  return -1

# Production metadata
type
  ProductionInfo* = object
    symbol*: Symbol
    fieldCount*: uint32
    childCount*: uint32
    fieldNames*: seq[string]

# Lex table data
type
  LexTransition* = object
    minChar*: int
    maxChar*: int
    nextState*: int
    isSeparator*: bool

  LexState* = object
    transitions*: seq[LexTransition]
    acceptSymbol*: int  # -1 if not accepting, else terminal symbol index

# Lexer runtime - DFA-based
type
  Point* = object
    row*: uint32
    column*: uint32

  InputEdit* = object
    startByte*: int
    oldEndByte*: int
    newEndByte*: int
    startPoint*: Point
    oldEndPoint*: Point
    newEndPoint*: Point

  Token* = object
    kind*: Symbol
    text*: string
    startPos*: int
    endPos*: int
    startPoint*: Point
    endPoint*: Point

  LexerObj* = object
    input*: string
    pos*: int
    row*: uint32
    col*: uint32
    scannerState*: pointer
    scannerCleanupFunc*: proc (state: pointer): void

  Lexer* = ref LexerObj

proc `=destroy`*(lexer: var LexerObj) =
  if lexer.scannerCleanupFunc != nil and lexer.scannerState != nil:
    try:
      lexer.scannerCleanupFunc(lexer.scannerState)
    except:
      discard
    lexer.scannerState = nil
    lexer.scannerCleanupFunc = nil

# Parser runtime types
type
  ParseNode* = ref object
    symbol*: Symbol
    children*: seq[ParseNode]
    token*: Token  # For terminal nodes
    startPos*: int
    endPos*: int
    startPoint*: Point
    endPoint*: Point

  Parser* = object
    lexer*: Lexer
    stacks*: seq[seq[tuple[state: int, node: ParseNode]]]
    lookahead*: Token

# TSLexer type (matches tree-sitter C API)
type
  TSSymbol* = uint16

  TSLexer* {.bycopy.} = object
    lookahead*: int32
    result_symbol*: TSSymbol
    advance*: proc(self: ptr TSLexer, skip: bool) {.cdecl.}
    mark_end*: proc(self: ptr TSLexer) {.cdecl.}
    get_column*: proc(self: ptr TSLexer): uint32 {.cdecl.}
    is_at_included_range_start*: proc(self: ptr TSLexer): bool {.cdecl.}
    eof*: proc(self: ptr TSLexer): bool {.cdecl.}
    log*: proc(self: ptr TSLexer, msg: cstring) {.cdecl, varargs.}

# Compact Helpers
func s*(state: uint32): ParseAction =
  ParseAction(kind: pakShift, shiftState: state)

func r*(sym: Symbol, count: uint32, prec: int32, staticPrec: int32, assoc: Option[Associativity], dynPrec: int32): ParseAction =
  ParseAction(kind: pakReduce, reduceSymbol: sym, reduceCount: count, reducePrecedence: prec, reduceStaticPrecedence: staticPrec, reduceAssociativity: assoc, reduceDynamicPrecedence: dynPrec)

func acc*(): ParseAction = ParseAction(kind: pakAccept)
func err*(): ParseAction = ParseAction(kind: pakError)

func t*(i: int): Symbol = terminal(i)
func nt*(i: int): Symbol = nonTerminal(i)

proc hash*(node: ParseNode): Hash =
  result = hash(node.symbol)
  for child in node.children:
    result = result !& hash(result)
  result = result !& hash(node.token)
  result = result !& hash(node.startPos)
  result = result !& hash(node.endPos)

import macros
macro debugEchoMsg*(msg: varargs[untyped]): untyped =
  result = quote do:
    when defined(debug):
      # echo fmt"""[DEBUG][{now().format("yyyyMMdd HH:mm:ss")}] """, `msg`
      echo fmt"""[DEBUG] """, `msg`
    else:
      discard