## C-compatible type definitions for tree-sitter interface

## These types match the C API from tree_sitter/parser.h

type
  TSStateId* = uint16
  TSSymbol* = uint16
  TSFieldId* = uint16

  TSParseActionType* = enum
    TSParseActionTypeShift = 0
    TSParseActionTypeReduce = 1
    TSParseActionTypeAccept = 2
    TSParseActionTypeRecover = 3

  TSLexer* {.importc: "TSLexer", header: "tree_sitter/parser.h", bycopy.} = object
    lookahead* {.importc: "lookahead".}: int32
    result_symbol* {.importc: "result_symbol".}: TSSymbol
    advance* {.importc: "advance".}: proc(lexer: ptr TSLexer, skip: bool) {.cdecl.}
    mark_end* {.importc: "mark_end".}: proc(lexer: ptr TSLexer) {.cdecl.}
    get_column* {.importc: "get_column".}: proc(lexer: ptr TSLexer): uint32 {.cdecl.}
    is_at_included_range_start* {.importc: "is_at_included_range_start".}: proc(lexer: ptr TSLexer): bool {.cdecl.}
    eof* {.importc: "eof".}: proc(lexer: ptr TSLexer): bool {.cdecl.}
    log* {.importc: "log".}: proc(lexer: ptr TSLexer, format: cstring) {.cdecl, varargs.}

  TSParseAction* {.importc: "TSParseAction", header: "tree_sitter/parser.h", bycopy.} = object
    `type`* {.importc: "type".}: uint8
    when false:  # Union fields - use case statement
      shift*: object
        `type`*: uint8
        state*: TSStateId
        extra*: bool
        repetition*: bool
      reduce*: object
        `type`*: uint8
        child_count*: uint8
        symbol*: TSSymbol
        dynamic_precedence*: int16
        production_id*: uint16

  TSLexMode* {.importc: "TSLexMode", header: "tree_sitter/parser.h", bycopy.} = object
    lex_state* {.importc: "lex_state".}: uint16
    external_lex_state* {.importc: "external_lex_state".}: uint16

  TSLexerMode* {.importc: "TSLexerMode", header: "tree_sitter/parser.h", bycopy.} = object
    lex_state* {.importc: "lex_state".}: uint16
    external_lex_state* {.importc: "external_lex_state".}: uint16
    reserved_word_set_id* {.importc: "reserved_word_set_id".}: uint16

  TSParseActionEntry* {.importc: "TSParseActionEntry", header: "tree_sitter/parser.h", bycopy.} = object
    action*: TSParseAction
    entry*: object
      count*: uint8
      reusable*: bool

  TSLanguage* {.importc: "TSLanguage", header: "tree_sitter/api.h", incompleteStruct.} = object
    # Opaque type - we only use pointers to it

  TSLanguageMetadata* {.importc: "TSLanguageMetadata", header: "tree_sitter/api.h", bycopy.} = object
    major_version* {.importc: "major_version".}: uint8
    minor_version* {.importc: "minor_version".}: uint8
    patch_version* {.importc: "patch_version".}: uint8

  TSFieldMapEntry* {.importc: "TSFieldMapEntry", header: "tree_sitter/parser.h", bycopy.} = object
    field_id* {.importc: "field_id".}: TSFieldId
    child_index* {.importc: "child_index".}: uint8
    inherited* {.importc: "inherited".}: bool

  TSMapSlice* {.importc: "TSMapSlice", header: "tree_sitter/parser.h", bycopy.} = object
    index* {.importc: "index".}: uint16
    length* {.importc: "length".}: uint16

  TSSymbolMetadata* {.importc: "TSSymbolMetadata", header: "tree_sitter/parser.h", bycopy.} = object
    visible* {.importc: "visible".}: bool
    named* {.importc: "named".}: bool
    supertype* {.importc: "supertype".}: bool

  TSExternalScannerState* = array[4, uint8]

  TSExternalScanner* {.importc: "TSExternalScanner", header: "tree_sitter/parser.h", bycopy.} = object
    states* {.importc: "states".}: ptr TSExternalScannerState
    create* {.importc: "create".}: proc(): pointer {.cdecl.}
    destroy* {.importc: "destroy".}: proc(payload: pointer) {.cdecl.}
    serialize* {.importc: "serialize".}: proc(payload: pointer, buffer: cstring): uint32 {.cdecl.}
    deserialize* {.importc: "deserialize".}: proc(payload: pointer, buffer: cstring, length: uint32) {.cdecl.}
    scan* {.importc: "scan".}: proc(payload: pointer, lexer: ptr TSLexer, valid_symbols: ptr bool): bool {.cdecl.}

const
  ts_builtin_sym_error* = TSSymbol(high(uint16))
  ts_builtin_sym_end* = TSSymbol(0)
  TREE_SITTER_SERIALIZATION_BUFFER_SIZE* = 1024

