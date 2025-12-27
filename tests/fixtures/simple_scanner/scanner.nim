## External scanner for simple_scanner grammar - Nim implementation
## This is equivalent to the C scanner in src/scanner.c

import treestand/parser_types

type
  TokenType = enum
    INDENT = 0
    DEDENT = 1
    NEWLINE = 2

  Scanner = ref object
    indentLevel: int
    indentStack: seq[int]

proc scanner_create*(): pointer {.exportc: "tree_sitter_simple_scanner_external_scanner_create".} =
  ## Create a new scanner instance
  let scanner = Scanner(
    indentLevel: 0,
    indentStack: @[0]  # Base indentation
  )
  return cast[pointer](scanner)

proc scanner_destroy*(payload: pointer) {.exportc: "tree_sitter_simple_scanner_external_scanner_destroy".} =
  ## Destroy the scanner instance
  if payload != nil:
    let scanner = cast[Scanner](payload)
    # Nim's GC will handle cleanup
    discard

proc scanner_serialize*(payload: pointer, buffer: cstring): cuint {.exportc: "tree_sitter_simple_scanner_external_scanner_serialize".} =
  ## Serialize scanner state for incremental parsing
  if payload == nil:
    return 0
  
  let scanner = cast[Scanner](payload)
  if scanner.indentStack.len > 0:
    let size = min(scanner.indentStack.len * sizeof(int), 256)
    if size > 0:
      copyMem(buffer, addr scanner.indentStack[0], size)
      return size.cuint
  return 0

proc scanner_deserialize*(payload: pointer, buffer: cstring, length: cuint) {.exportc: "tree_sitter_simple_scanner_external_scanner_deserialize".} =
  ## Deserialize scanner state for incremental parsing
  if payload == nil or length == 0:
    return
  
  let scanner = cast[Scanner](payload)
  let count = length.int div sizeof(int)
  if count > 0:
    scanner.indentStack.setLen(count)
    copyMem(addr scanner.indentStack[0], buffer, length.int)

proc scanner_scan*(payload: pointer, lexer: ptr TSLexer, valid_symbols: ptr bool): bool {.exportc: "tree_sitter_simple_scanner_external_scanner_scan".} =
  ## Scan for external tokens
  if payload == nil:
    return false
  
  let scanner = cast[Scanner](payload)
  
  # Access valid_symbols array
  let validSymbols = cast[ptr UncheckedArray[bool]](valid_symbols)
  
  # Simple implementation: recognize newlines
  if validSymbols[NEWLINE.ord]:
    if lexer.lookahead == '\n'.int32 or lexer.lookahead == '\r'.int32:
      lexer.advance(lexer, false)
      if lexer.lookahead == '\n'.int32:
        lexer.advance(lexer, false)
      lexer.result_symbol = NEWLINE.uint16
      return true
  
  # Placeholder indent/dedent logic - just return false for now
  # Real implementation would count spaces and track indent stack
  
  return false
