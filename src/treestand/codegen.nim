## Code generation for parsers

import std/[strutils, strformat, os, options]
import grammar, nfa

type
  CodeGenError* = object of CatchableError

proc sanitizeSymbolName(name: string): string =
  ## Convert symbol name to valid Nim identifier
  ## Handles: quotes, special chars, regex patterns, Nim case-insensitivity
  if name.len == 0:
    return "Empty"
  
  # Strip surrounding quotes from string literals
  var cleanName = name
  if (cleanName.startsWith("'") and cleanName.endsWith("'")) or
     (cleanName.startsWith("\"") and cleanName.endsWith("\"")):
    cleanName = cleanName[1..^2]
  
  # Handle single special characters with readable names
  if cleanName.len == 1 or cleanName.len == 2:
    case cleanName
    of "=": return "Eq"
    of "==": return "EqEq"
    of "!=": return "NotEq"
    of "+": return "Plus"
    of "-": return "Minus"
    of "*": return "Star"
    of "/": return "Slash"
    of "%": return "Percent"
    of ":": return "Colon"
    of ";": return "Semi"
    of ",": return "Comma"
    of ".": return "Dot"
    of "!": return "Bang"
    of "?": return "Question"
    of "<": return "Lt"
    of ">": return "Gt"
    of "<=": return "LtEq"
    of ">=": return "GtEq"
    of "&": return "Amp"
    of "&&": return "And"
    of "|": return "Pipe"
    of "||": return "Or"
    of "^": return "Caret"
    of "~": return "Tilde"
    of "(": return "LParen"
    of ")": return "RParen"
    of "[": return "LBracket"
    of "]": return "RBracket"
    of "{": return "LBrace"
    of "}": return "RBrace"
    of "@": return "At"
    of "$": return "Dollar"
    of "#": return "Hash"
    of "\\": return "Backslash"
    of "->": return "Arrow"
    of "=>": return "FatArrow"
    of "\n": return "LF"
    of "\r": return "CR"
    of "\t": return "Tab"
    else: discard
  
  # Handle regex patterns - extract meaningful part
  if cleanName.startsWith("/") and cleanName.endsWith("/"):
    cleanName = cleanName[1..^2]
    # Simple pattern extraction
    if cleanName.contains("[a-zA-Z_]"):
      return "Identifier"
    elif cleanName.contains("[0-9]"):
      return "Number"
    else:
      return "Pattern"
  
  # For other names, convert to valid identifier
  # Replace special chars but keep alphanumerics
  result = ""
  var lastWasUnderscore = false
  
  for i, c in cleanName:
    if c.isAlphaNumeric():
      result.add(c)
      lastWasUnderscore = false
    elif c == '_':
      # Keep underscores but avoid consecutive ones
      if not lastWasUnderscore and result.len > 0:
        result.add('_')
        lastWasUnderscore = true
    else:
      # Replace other special chars with underscore
      if not lastWasUnderscore and result.len > 0:
        result.add('_')
        lastWasUnderscore = true
  
  # Remove trailing underscores
  while result.len > 0 and result[^1] == '_':
    result = result[0..^2]
  
  # Ensure it doesn't start with a digit
  if result.len > 0 and result[0].isDigit():
    result = "T" & result
  
  # Ensure we have something
  if result.len == 0:
    result = "GrammarSymbol"
  
  # Capitalize first letter to avoid case-insensitivity conflicts
  # Nim treats foo_bar and fooBar as the same, so we use PascalCase
  if result.len > 0:
    result[0] = result[0].toUpperAscii()
  
  return result

proc generateSymbolEnum*(syntaxGrammar: SyntaxGrammar, lexicalGrammar: LexicalGrammar, externalTokens: seq[string]): string =
  ## Generate symbol enum for the parser
  ## GrammarSymbol ordering:
  ## 1. EOF symbol (index 0)
  ## 2. Regular terminal symbols from lexical grammar
  ## 3. External scanner symbols (if any)
  ## 4. Non-terminal symbols from syntax grammar
  
  var lines: seq[string] = @[]
  
  # First, define the GrammarSymbol base types
  lines.add("type")
  # Then terminal enum
  lines.add("  TerminalSymbol* = enum")
  lines.add("    tsEOF = 0")
  
  var termIndex = 1
  var usedNames: seq[string] = @["EOF", "Eof"]
  
  for variable in lexicalGrammar.variables:
    var sanitizedName = sanitizeSymbolName(variable.name)
    
    # Handle name collisions
    if sanitizedName in usedNames:
      sanitizedName = sanitizedName & "_" & $termIndex
    
    usedNames.add(sanitizedName)
    
    lines.add(&"    ts{sanitizedName} = {termIndex}")
    inc termIndex
  
  # External tokens
  if externalTokens.len > 0:
    for extToken in externalTokens:
      if extToken == "_eof": continue # Skip implicit _eof, it maps to tsEOF (0)
      
      var sanitized = sanitizeSymbolName(extToken)
      
      # Handle name collisions
      if sanitized in usedNames:
        sanitized = sanitized & "_" & $termIndex
      
      usedNames.add(sanitized)
      
      let enumName = "ts" & sanitized
      lines.add(&"    {enumName} = {termIndex}")
      inc termIndex
  
  lines.add("")
  
  # Non-terminal symbols enum
  lines.add("  NonTerminalSymbol* = enum")
  
  usedNames.setLen(0)
  for i, variable in syntaxGrammar.variables:
    var sanitizedName = sanitizeSymbolName(variable.name)
    
    if sanitizedName in usedNames:
      sanitizedName = sanitizedName & "_" & $i
    
    usedNames.add(sanitizedName)
    
    lines.add(&"    nt{sanitizedName} = {i}")
  lines.add("")
  
  # Add symbol name arrays for debugging
  lines.add("# GrammarSymbol names for debugging and error messages")
  lines.add("const terminalSymbolNames* = [")
  lines.add("  \"EOF\",")
  for variable in lexicalGrammar.variables:
    lines.add(&"  {variable.name.escape},")
  
  for extToken in externalTokens:
    lines.add(&"  {extToken.escape},")
  lines.add("]")
  lines.add("")
  
  lines.add("const nonTerminalSymbolNames* = [")
  for variable in syntaxGrammar.variables:
    lines.add(&"  {variable.name.escape},")
  lines.add("]")
  lines.add("")
  
  lines.add("const terminalSymbolMetadata* = [")
  lines.add("  parser_types.SymbolMetadata(named: false),  # EOF")
  for variable in lexicalGrammar.variables:
    # Lexical tokens from regex are typically named (identifiers, numbers)
    # String literals in quotes are anonymous
    let isNamed = if variable.name.startsWith("'") or variable.name.startsWith("\""): "false" else: "true"
    # Escape the variable name in the comment to prevent special chars (like newlines) from breaking the code
    lines.add(&"  parser_types.SymbolMetadata(named: {isNamed}),  # {variable.name.escape}")
  for extToken in externalTokens:
    # External tokens starting with _ are typically auxiliary/hidden
    let isNamed = if extToken.startsWith("_"): "false" else: "true"
    lines.add(&"  parser_types.SymbolMetadata(named: {isNamed}),  # {extToken.escape}")
  lines.add("]")
  lines.add("")
  
  lines.add("const nonTerminalSymbolMetadata* = [")
  for variable in syntaxGrammar.variables:
    # Non-terminals are always named
    lines.add(&"  parser_types.SymbolMetadata(named: true),  # {variable.name}")
  lines.add("]")
  lines.add("")
  
  result = lines.join("\n")

proc generateExternalScannerBindings*(grammarName: string, grammarPath: string, externalTokens: seq[string]): string =
  ## Generate external scanner bindings if grammar has external tokens
  if externalTokens.len == 0:
    return ""
  
  var lines: seq[string] = @[]
  
  # Compile pragma with absolute path
  # Try src/scanner.c first (standard tree-sitter convention)
  let grammarDir = grammarPath
  let scannerStrPath = grammarDir / "src"
  var scannerPath = scannerStrPath / "scanner.c"
  if not fileExists(scannerPath):
    if fileExists(scannerStrPath / "scanner.cc"):
      scannerPath = scannerStrPath / "scanner.cc"
    elif fileExists(grammarDir / "scanner.c"):
      scannerPath = grammarDir / "scanner.c"
    elif fileExists(grammarDir / "scanner.cc"):
      scannerPath = grammarDir / "scanner.cc"
    else:
      raise newException(ValueError, "External scanner not found")
  
  lines.add(&"# Compile external scanner")
  lines.add(&"{{.passC: \"-I{grammarPath}\".}}")
  lines.add(&"{{.passC: \"-I{scannerStrPath}\".}}")
  lines.add(&"{{.compile: \"{scannerPath}\".}}")
  lines.add("")
  
  # # TSLexer type definition
  # lines.add("# TSLexer type (matches tree-sitter C API)")
  # lines.add("type")
  # lines.add("  TSSymbol* = uint16")
  # lines.add("")
  # lines.add("  TSLexer* {.bycopy.} = object")
  # lines.add("    lookahead*: int32")
  # lines.add("    result_symbol*: TSSymbol")
  # lines.add("    advance*: proc(self: ptr TSLexer, skip: bool) {.cdecl.}")
  # lines.add("    mark_end*: proc(self: ptr TSLexer) {.cdecl.}")
  # lines.add("    get_column*: proc(self: ptr TSLexer): uint32 {.cdecl.}")
  # lines.add("    is_at_included_range_start*: proc(self: ptr TSLexer): bool {.cdecl.}")
  # lines.add("    eof*: proc(self: ptr TSLexer): bool {.cdecl.}")
  # lines.add("    log*: proc(self: ptr TSLexer, msg: cstring) {.cdecl, varargs.}")
  # lines.add("")
  
  # Scanner function imports
  let prefix = &"tree_sitter_{grammarName}_external_scanner"
  lines.add("# External scanner functions")
  lines.add(&"proc scanner_create*(): pointer {{.importc: \"{prefix}_create\".}}")
  lines.add(&"proc scanner_destroy*(payload: pointer) {{.importc: \"{prefix}_destroy\".}}")
  lines.add(&"proc scanner_scan*(payload: pointer, lexer: ptr TSLexer, valid_symbols: ptr bool): bool {{.importc: \"{prefix}_scan\".}}")
  lines.add(&"proc scanner_serialize*(payload: pointer, buffer: cstring): cuint {{.importc: \"{prefix}_serialize\".}}")
  lines.add(&"proc scanner_deserialize*(payload: pointer, buffer: cstring, length: cuint) {{.importc: \"{prefix}_deserialize\".}}")
  
  result = lines.join("\n")

proc generateParseTableCode*(parseTable: BuildParseTable, lexicalGrammar: LexicalGrammar, externalTokens: seq[string]): string =
  ## Generate parse table data as Nim code using COMPACT arrays
  var lines: seq[string] = @[]
  
  let numTerminals = lexicalGrammar.variables.len
  let externalTokenBase = numTerminals + 1
  
  # Flatten dictionaries into single lists
  var allActions: seq[string] = @[]
  var allGotos: seq[string] = @[]
  var indexEntries: seq[string] = @[]
  
  var currentActionIdx = 0
  var currentGotoIdx = 0
  
  lines.add("# Compact Parse Table Arrays")
  
  # Accumulate data
  for i, entry in parseTable.entries:
    let actionStart = currentActionIdx
    
    # Process Actions
    for (sym, action) in entry.actionMap:
      # Symbol
      # Symbol
      var symCode = ""
      case sym.kind
      of stNonTerminal: symCode = &"parser_types.nt({sym.index})"
      of stTerminal: symCode = &"parser_types.t({sym.index + 1})"
      of stExternal: 
         if externalTokens.len > 0 and externalTokens[0] == "_eof":
           if sym.index == 0:
              symCode = "parser_types.t(0)"
           else:
              symCode = &"parser_types.t({externalTokenBase + sym.index.int - 1})"
         else:
           symCode = &"parser_types.t({externalTokenBase + sym.index.int})" 
      else: symCode = "parser_types.t(0)" 
      
      # Action
      var actionCode = ""
      case action.kind
      of bpakShift:
        actionCode = &"parser_types.s({action.shiftState}u32)"
      of bpakReduce:
        let reduceSym = &"parser_types.nt({action.reduceSymbol.index})"
        let assocName = if action.reduceAssociativity.isSome: 
          let val = action.reduceAssociativity.get
          let str = case val
            of gaLeft: "assocLeft"
            of gaRight: "assocRight"
          "some(parser_types." & str & ")" 
        else: 
          "none(parser_types.Associativity)"
        
        actionCode = &"parser_types.r({reduceSym}, {action.reduceCount}u32, {action.reducePrecedence}, {action.reduceStaticPrecedence}, {assocName}, {action.reducePrecedence})"
      of bpakAccept:
        actionCode = "parser_types.acc()"
      of bpakError:
        actionCode = "parser_types.err()"
      of bpakShiftExtra:
        actionCode = "parser_types.ParseAction(kind: parser_types.pakShiftExtra)"
      of bpakSplit:
        # Split not supported in compact mode yet (rarely used?)
        # Fallback to error or full parsing?
        # Tree-sitter splits are rare. If we have them, we might need a workaround.
        # For now, let's treat it as error to warn.
        actionCode = "parser_types.err() # Split action not supported in compact mode"

        
      allActions.add(&"  ({symCode}, {actionCode}),")
      inc currentActionIdx
      
    let actionLen = currentActionIdx - actionStart
    let gotoStart = currentGotoIdx
    
    # Process Gotos
    for (sym, state) in entry.gotoMap:
      let symCode = &"parser_types.nt({sym.index})"
      allGotos.add(&"  ({symCode}, {state}u32),")
      inc currentGotoIdx
      
    let gotoLen = currentGotoIdx - gotoStart
    
    # Index Entry
    indexEntries.add(&"  (actionStart: {actionStart}i32, actionLen: {actionLen}i32, gotoStart: {gotoStart}i32, gotoLen: {gotoLen}i32, lexState: {entry.lexState}i32),")
  
  # Emit Action Array
  lines.add("const parseTableActions* = @[")
  lines.add(allActions.join("\n"))
  lines.add("]")
  lines.add("")
  
  # Emit Goto Array
  lines.add("const parseTableGotos* = @[")
  lines.add(allGotos.join("\n"))
  lines.add("]")
  lines.add("")
  
  # Emit Index Array
  lines.add("const parseTableIndex* = [")
  lines.add(indexEntries.join("\n"))
  lines.add("]")
  lines.add("")
  
  # Add production info if present (unchanged)
  if parseTable.productionInfos.len > 0:
    lines.add("const productionInfos* = @[")
    for info in parseTable.productionInfos:
      lines.add("  parser_types.ProductionInfo(")
      let symCode = &"parser_types.nt({info.symbol.index})"
      lines.add(&"    symbol: {symCode},")
      lines.add(&"    fieldCount: {info.fieldCount}u32,")
      lines.add(&"    childCount: {info.childCount}u32,")
      lines.add("    fieldNames: @[")
      for fname in info.fieldNames:
        lines.add(&"      \"{fname}\",")
      lines.add("    ]")
      lines.add("  ),")
    lines.add("]")
    lines.add("")
  
  result = lines.join("\n")


proc generateLexTableCode*(lexTable: BuildLexTable): string =
  ## Generate lex table data as Nim code
  var lines: seq[string] = @[]
  
  # Generate lex states
  if lexTable.states.len == 0:
    # Empty seq
    lines.add("const lexStates*: seq[parser_types.LexState] = @[]")
  else:
    lines.add("const lexStates* = @[")
    for state in lexTable.states:
      lines.add("  parser_types.LexState(")
      
      # Transitions
      lines.add("    transitions: @[")
      for trans in state.transitions:
        # Extract character ranges from CharacterSet
        for charRange in trans.characters.ranges:
          lines.add("      parser_types.LexTransition(")
          lines.add(&"        minChar: {charRange.start},") 
          # tree-sitter ranges are exclusive, so end - 1 for inclusive max
          let maxChar = if charRange.`end` > 0: charRange.`end` - 1 else: 0
          lines.add(&"        maxChar: {maxChar},")
          lines.add(&"        nextState: {trans.state},")
          lines.add(&"        isSeparator: {trans.isSeparator}")
          lines.add("      ),")
      lines.add("    ],")
      
      # Accept symbol
      let acceptSym = if state.accept.isSome(): state.accept.get() else: -1
      lines.add(&"    acceptSymbol: {acceptSym}")
      
      lines.add("  ),")
    lines.add("]")
  
  result = lines.join("\n")

proc generateLexerRuntime*(lexTable: BuildLexTable, lexicalGrammar: LexicalGrammar, externalTokens: seq[string], extras: seq[GrammarSymbol]): string =
  let hasExternalTokens = externalTokens.len > 0
  let hasEofAt0 = externalTokens.len > 0 and externalTokens[0] == "_eof"
  ## Generate DFA-based lexer runtime
  var lines: seq[string] = @[]
  
  lines.add("proc newLexer*(input: string): Lexer =")
  lines.add("  result = Lexer(input: input, pos: 0, row: 0, col: 0)")
  if hasExternalTokens:
    lines.add("  result.scannerState = scanner_create()")
    lines.add("  result.scannerCleanupFunc = scanner_destroy")
  lines.add("")
  
  # Add cleanup
  if hasExternalTokens:    
    # Add serialization support for incremental parsing
    lines.add("proc serializeScannerState*(lexer: Lexer): string =")
    lines.add("  ## Serialize external scanner state for incremental parsing")
    lines.add("  if lexer.scannerState == nil:")
    lines.add("    return \"\"")
    lines.add("  var buffer: array[256, char]")
    lines.add("  let size = scanner_serialize(lexer.scannerState, cast[cstring](addr buffer[0]))")
    lines.add("  if size > 0:")
    lines.add("    result = newString(size.int)")
    lines.add("    copyMem(addr result[0], addr buffer[0], size.int)")
    lines.add("  else:")
    lines.add("    result = \"\"")
    lines.add("")
    
    lines.add("proc deserializeScannerState*(lexer: Lexer, state: string) =")
    lines.add("  ## Restore external scanner state for incremental parsing")
    lines.add("  if lexer.scannerState != nil and state.len > 0:")
    lines.add("    scanner_deserialize(lexer.scannerState, cstring(state), state.len.cuint)")
    lines.add("")
  
  # Add external scanner callbacks at module level (before nextToken)
  if hasExternalTokens:
    lines.add("# External scanner context (for C callbacks)")
    lines.add("var g_scannerLexer {.threadvar.}: Lexer")
    lines.add("var g_scannerPos {.threadvar.}: int")
    lines.add("var g_scannerMarkedPos {.threadvar.}: int")
    lines.add("")
    lines.add("# C callbacks for TSLexer (must be module-level for {.cdecl.})")
    lines.add("proc scannerAdvance(self: ptr TSLexer, skip: bool) {.cdecl.} =")
    lines.add("  if g_scannerPos < g_scannerLexer.input.len:")
    lines.add("    let len = runeLenAt(g_scannerLexer.input, g_scannerPos)")
    lines.add("    g_scannerPos += len")
    lines.add("    if g_scannerPos < g_scannerLexer.input.len:")
    lines.add("      self.lookahead = runeAt(g_scannerLexer.input, g_scannerPos).int32")
    lines.add("    else:")
    lines.add("      self.lookahead = 0")
    lines.add("  else:")
    lines.add("    self.lookahead = 0")
    lines.add("proc scannerMarkEnd(self: ptr TSLexer) {.cdecl.} =")
    lines.add("  g_scannerMarkedPos = g_scannerPos")
    lines.add("")
    lines.add("proc scannerGetColumn(self: ptr TSLexer): uint32 {.cdecl.} =")
    lines.add("  var col = 0u32")
    lines.add("  var pos = g_scannerPos - 1")
    lines.add("  while pos >= 0 and g_scannerLexer.input[pos] != '\\n':")
    lines.add("    col += 1")
    lines.add("    pos -= 1")
    lines.add("  return col")
    lines.add("")
    lines.add("proc scannerIsAtIncludedRangeStart(self: ptr TSLexer): bool {.cdecl.} =")
    lines.add("  return true")
    lines.add("")
    lines.add("proc scannerEof(self: ptr TSLexer): bool {.cdecl.} =")
    lines.add("  return g_scannerPos >= g_scannerLexer.input.len")
    lines.add("")
    lines.add("proc scannerLog(self: ptr TSLexer, msg: cstring) {.cdecl, varargs.} =")
    lines.add("  echo msg")
    lines.add("")
  
  # Helper to advance lexer and update row/col
  lines.add("proc advance(lexer: Lexer, count: int = 1) =")
  lines.add("  for _ in 0..<count:")
  lines.add("    if lexer.pos < lexer.input.len:")
  lines.add("      if lexer.input[lexer.pos] == '\\n':")
  lines.add("        inc lexer.row")
  lines.add("        lexer.col = 0")
  lines.add("      else:")
  lines.add("        inc lexer.col")
  lines.add("      inc lexer.pos")
  lines.add("")

  lines.add("proc nextToken*(lexer: Lexer, validExternalSymbols: set[int16] = {}, lexState: int = 0): Token =")
  
  if hasExternalTokens:
    lines.add("  # Try external scanner first if we have external tokens")
    lines.add("  if validExternalSymbols.card > 0:")
    lines.add("    # Setup global context for C callbacks")
    lines.add("    g_scannerLexer = lexer")
    lines.add("    g_scannerPos = lexer.pos")
    lines.add("    g_scannerMarkedPos = lexer.pos")
    lines.add("    ")
    lines.add("    # Setup valid symbols array")
    lines.add("    var validSymbolsArray: array[256, bool]")
    lines.add("    for i in 0..<256:")
    lines.add("      validSymbolsArray[i] = false")
    lines.add(&"    const externalTokenBase = {lexicalGrammar.variables.len + 1}")
    lines.add("    # Convert terminal indices to external scanner indices (0-based)")
    lines.add("    for termIdx in validExternalSymbols:")
    lines.add("      if termIdx >= externalTokenBase:")
    if hasEofAt0:
      lines.add("        # shifted by 1 because _eof (0) was skipped in TerminalSymbol but exists in scanner")
      lines.add("        let extIdx = termIdx - externalTokenBase + 1")
    else:
      lines.add("        let extIdx = termIdx - externalTokenBase")
    
    lines.add("        if extIdx >= 0 and extIdx < 256:")
    lines.add("          validSymbolsArray[extIdx] = true")
    lines.add("    ")
    lines.add("    # Create TSLexer with C callbacks")
    lines.add("    var tsLexer: TSLexer")
    lines.add("    tsLexer.lookahead = if g_scannerPos < lexer.input.len: runeAt(lexer.input, g_scannerPos).int32 else: 0")
    lines.add("    tsLexer.result_symbol = 0")
    lines.add("    tsLexer.advance = scannerAdvance")
    lines.add("    tsLexer.mark_end = scannerMarkEnd")
    lines.add("    tsLexer.get_column = scannerGetColumn")
    lines.add("    tsLexer.is_at_included_range_start = scannerIsAtIncludedRangeStart")
    lines.add("    tsLexer.eof = scannerEof")
    lines.add("    tsLexer.log = scannerLog")
    lines.add("    ")
    lines.add("    # Call scanner")
    lines.add("    if scanner_scan(lexer.scannerState, addr tsLexer, addr validSymbolsArray[0]):")
    lines.add("      debugEchoMsg \"[SCANNER] Scanned symbol: \", tsLexer.result_symbol")
    lines.add(&"      const externalTokenBase = {lexicalGrammar.variables.len + 1}")
    lines.add("      ")
    
    # Check if _eof needs special handling (using hasEofAt0 defined above)
    
    if hasEofAt0:
       lines.add("      # Logic to handle _eof (external 0) -> tsEOF (0)")
       lines.add("      # And offset others by -1 relative to base")
       lines.add("      var actualSymbol = 0")
       lines.add("      if tsLexer.result_symbol == 0: # END_OF_FILE / _eof")
       lines.add("         actualSymbol = 0")
       lines.add("      else:")
       lines.add("         actualSymbol = externalTokenBase + tsLexer.result_symbol.int - 1")
    else:
       lines.add("      let actualSymbol = externalTokenBase + tsLexer.result_symbol.int")

    lines.add("      ")
    lines.add("      var isValid = false")
    lines.add("      if actualSymbol < 32767 and actualSymbol.int16 in validExternalSymbols:")
    lines.add("        isValid = true")
    lines.add("      ")
    lines.add("      if isValid:")
    lines.add("        let startPoint = Point(row: lexer.row, column: lexer.col)")
    lines.add("        var endPos = g_scannerMarkedPos")
    
    var extNamesCode = "@["
    for i, name in externalTokens:
      if i > 0: extNamesCode.add(", ")
      extNamesCode.add("\"" & name.escape("", "") & "\"")
    extNamesCode.add("]")
    
    lines.add(&"        const externalNames = {extNamesCode}")
    lines.add("        let extName = externalNames[tsLexer.result_symbol.int]")
    lines.add("        ")
    lines.add("        if endPos <= lexer.pos and g_scannerPos > lexer.pos:")
    lines.add("          let nameUp = extName.toUpperAscii")
    lines.add("          if not (nameUp.contains(\"INDENT\") or nameUp.contains(\"DEDENT\") or nameUp.contains(\"NEWLINE\")):")
    lines.add("            endPos = g_scannerPos")
    lines.add("        ")
    lines.add("        let text = if endPos > lexer.pos: lexer.input[lexer.pos..<endPos] else: \"\"")
    lines.add("        ")
    lines.add("        # Update row/col for external scanner consumption")
    lines.add("        # We must scan the consumed text to update row/col")
    lines.add("        let consumedLen = endPos - lexer.pos")
    lines.add("        advance(lexer, consumedLen)")
    lines.add("        ")
    lines.add("        let endPoint = Point(row: lexer.row, column: lexer.col)")
    lines.add("        ")
    lines.add("        return Token(kind: terminal(actualSymbol), text: text, startPos: lexer.pos - text.len, endPos: endPos, startPoint: startPoint, endPoint: endPoint)")
    lines.add("  ")

  lines.add("  # Skip whitespace and extras (preserve start for scanning)")
  lines.add("  let startPosBeforeSkip {.used.} = lexer.pos")
  lines.add("  ")
  lines.add("  # Check for EOF")
  lines.add("  if lexer.pos >= lexer.input.len:")
  lines.add("    let pt = Point(row: lexer.row, column: lexer.col)")
  lines.add("    return Token(kind: terminal(int(tsEOF)), text: \"\", startPos: lexer.pos, endPos: lexer.pos, startPoint: pt, endPoint: pt)")
  lines.add("  ")

  if extras.len > 0:
    lines.add("  # Skip extras (tokens defined as skippable in grammar)")
    lines.add("  var skipAgain = true")
    lines.add("  while skipAgain:")
    lines.add("    skipAgain = false")
    lines.add("    ")
    lines.add("    ")
    lines.add("    if lexer.pos >= lexer.input.len:")
    lines.add("      let pt = Point(row: lexer.row, column: lexer.col)")
    lines.add("      return Token(kind: terminal(int(tsEOF)), text: \"\", startPos: lexer.pos, endPos: lexer.pos, startPoint: pt, endPoint: pt)")
    lines.add("    ")
    lines.add("    # Try to match each extra symbol")
    for extra in extras:
      let extraTerminalId = extra.index + 1
      lines.add(&"    # Try to skip extra: terminal({extraTerminalId})")
      lines.add("    block tryExtra" & $extraTerminalId & ":")
      lines.add("      let extraStartPos = lexer.pos")
      lines.add("      let extraStartRow = lexer.row")
      lines.add("      let extraStartCol = lexer.col")
      lines.add("      var extraState = lexState")
      lines.add("      var extraLastAccept = -1")
      lines.add("      var extraAcceptPos = extraStartPos")
      lines.add("      var extraAcceptRow = extraStartRow")
      lines.add("      var extraAcceptCol = extraStartCol")
      lines.add("      ")
      lines.add("      while lexer.pos < lexer.input.len:")
      lines.add("        let ch = lexer.input[lexer.pos].int")
      lines.add("        ")
      lines.add("        if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:")
      lines.add("          extraLastAccept = lexStates[extraState].acceptSymbol")
      lines.add("          extraAcceptPos = lexer.pos")
      lines.add("          extraAcceptRow = lexer.row")
      lines.add("          extraAcceptCol = lexer.col")
      lines.add("        ")
      lines.add("        var foundTrans = false")
      lines.add("        if extraState < lexStates.len:")
      lines.add("          for trans in lexStates[extraState].transitions:")
      lines.add("            if ch >= trans.minChar and ch <= trans.maxChar:")
      lines.add("              extraState = trans.nextState")
      lines.add("              advance(lexer)")
      lines.add("              foundTrans = true")
      lines.add("              break")
      lines.add("        ")
      lines.add("        if not foundTrans:")
      lines.add("          break")
      lines.add("      ")
      lines.add("      if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:")
      lines.add("        extraLastAccept = lexStates[extraState].acceptSymbol")
      lines.add("        extraAcceptPos = lexer.pos")
      lines.add("        extraAcceptRow = lexer.row")
      lines.add("        extraAcceptCol = lexer.col")
      lines.add("      ")
      lines.add(&"      if extraLastAccept == {extra.index}:")
      lines.add("        debugEchoMsg \"Matched extra {extra.index}: \" & lexer.input[startPosBeforeSkip..<extraAcceptPos]")
      lines.add("        lexer.pos = extraAcceptPos")
      lines.add("        lexer.row = extraAcceptRow")
      lines.add("        lexer.col = extraAcceptCol")
      lines.add("        skipAgain = true")
      lines.add("        break tryExtra" & $extraTerminalId)
      lines.add("      else:")
      lines.add("        # Not this extra, restore position")
      lines.add("        lexer.pos = extraStartPos")
      lines.add("        lexer.row = extraStartRow")
      lines.add("        lexer.col = extraStartCol")
    lines.add("  ")
  
  lines.add("  let startPos = lexer.pos")
  lines.add("  let startPoint = Point(row: lexer.row, column: lexer.col)")
  
  lines.add("  var state = lexState")
  lines.add("  var lastAcceptPos = startPos")
  lines.add("  var lastAcceptRow = lexer.row")
  lines.add("  var lastAcceptCol = lexer.col")
  lines.add("  var lastAcceptSymbol = -1")
  lines.add("  ")
  lines.add("  # DFA traversal")
  lines.add("  while lexer.pos < lexer.input.len:")
  lines.add("    let ch = lexer.input[lexer.pos].int")
  lines.add("    ")
  lines.add("    # Check if current state accepts")
  lines.add("    if state < lexStates.len and lexStates[state].acceptSymbol >= 0:")
  lines.add("      lastAcceptPos = lexer.pos")
  lines.add("      lastAcceptRow = lexer.row")
  lines.add("      lastAcceptCol = lexer.col")
  lines.add("      lastAcceptSymbol = lexStates[state].acceptSymbol")
  lines.add("    ")
  lines.add("    # Find matching transition")
  lines.add("    var foundTransition = false")
  lines.add("    if state < lexStates.len:")
  lines.add("      for trans in lexStates[state].transitions:")
  lines.add("        if ch >= trans.minChar and ch <= trans.maxChar:")
  lines.add("          state = trans.nextState")
  lines.add("          advance(lexer)")
  lines.add("          foundTransition = true")
  lines.add("          break")
  lines.add("    ")
  lines.add("    # No transition found - stop")
  lines.add("    if not foundTransition:")
  lines.add("      break")
  lines.add("  ")
  lines.add("  # Check final state")
  lines.add("  if state < lexStates.len and lexStates[state].acceptSymbol >= 0:")
  lines.add("    lastAcceptPos = lexer.pos")
  lines.add("    lastAcceptRow = lexer.row")
  lines.add("    lastAcceptCol = lexer.col")
  lines.add("    lastAcceptSymbol = lexStates[state].acceptSymbol")
  lines.add("  ")
  lines.add("  # Return token if we accepted")
  lines.add("  if lastAcceptSymbol >= 0:")
  lines.add("    let text = lexer.input[startPos..<lastAcceptPos]")
  lines.add("    lexer.pos = lastAcceptPos")
  lines.add("    lexer.row = lastAcceptRow")
  lines.add("    lexer.col = lastAcceptCol")
  lines.add("    return Token(kind: terminal(lastAcceptSymbol + 1), text: text, startPos: startPos, endPos: lastAcceptPos, startPoint: startPoint, endPoint: Point(row: lexer.row, column: lexer.col))")
  lines.add("  ")
  lines.add("  # No match - error token (consume one char)")
  lines.add("  advance(lexer)")
  lines.add("  return Token(kind: terminal(int(tsEOF)), text: lexer.input[startPos..<lexer.pos], startPos: startPos, endPos: lexer.pos, startPoint: startPoint, endPoint: Point(row: lexer.row, column: lexer.col))")
  
  result = lines.join("\n")

proc generateParserRuntime*(lexicalGrammar: LexicalGrammar, extras: seq[GrammarSymbol]): string =
  ## Generate actual parser runtime implementation
  var lines: seq[string] = @[]
  
  let base = lexicalGrammar.variables.len + 1
  lines.add(&"const externalTokenBase* = {base}")
  
  # Collect ALL extras (both regular terminals and external tokens)
  # that need to be skipped via the pakShiftExtra mechanism
  var extExtras: seq[string] = @[]
  for sym in extras:
    # Check if it's a terminal (includes both regular and external terminals)
    if sym.kind == stTerminal:
      # Regular terminal extras appear with terminalIndex = sym.index + 1
      # (because terminal indices are 1-indexed, with 0 being EOF)
      extExtras.add($(sym.index + 1) & ".int16")
    elif sym.kind == stExternal:
      # External tokens come after all regular terminals
      extExtras.add($(base + sym.index.int) & ".int16")
      
  if extExtras.len > 0:
    lines.add("const externalExtraTokens* = {" & extExtras.join(", ") & "}")
  else:
    lines.add("const externalExtraTokens*: set[int16] = {}")
    
  lines.add("include treestand/parser_runtime")
  lines.add("")
  
  lines.add("proc newParser*(input: string): Parser =")
  lines.add("  var parser = Parser(")
  lines.add("    lexer: newLexer(input),")
  lines.add("    stacks: @[]")
  lines.add("  )")
  lines.add("  parser.stacks.add(@[(state: 0, node: ParseNode(nil))])  # Initial state")
  
  lines.add("  # Calculate valid external symbols for initial state (State 0)")
  lines.add("  var validExternal: set[int16] = {}")
  lines.add("  if parseTableIndex.len > 0:")
  lines.add("    let idx = parseTableIndex[0]")
  lines.add("    for i in 0 ..< idx.actionLen:")
  lines.add("      let (sym, _) = parseTableActions[idx.actionStart + i]")
  lines.add("      if sym.kind == skTerminal:")
  lines.add("        if sym.terminalIndex >= externalTokenBase:")
  lines.add("          validExternal.incl(sym.terminalIndex.int16)")
  
  lines.add("  parser.lookahead = parser.lexer.nextToken(validExternal, parseTableIndex[0].lexState)")
  lines.add("  return parser")
  lines.add("")
  
  lines.add("proc parse*(parser: var Parser): ParseNode =")
  lines.add("  return runGenericGLR(parser)")
  lines.add("")
  result = lines.join("\n")

proc snakeToCamel(name: string): string =
  var upcaseNext = true
  for idx, item in name:
    if idx == 0 or upcaseNext:
      result.add(item.toUpperAscii)
      upcaseNext = false
    elif item == '_':
      upcaseNext = true
    else:
      result.add(item)

proc generateParser*(grammarName: string, grammarPath: string, 
                     syntaxGrammar: SyntaxGrammar, 
                     lexicalGrammar: LexicalGrammar,
                     parseTable: BuildParseTable,
                     lexTable: BuildLexTable,
                     externalTokens: seq[string]): string =
  ## Generate complete parser code
  var lines: seq[string] = @[]
  
  lines.add(&"# Generated parser for {grammarName}")
  lines.add("# This file is auto-generated - do not edit")
  lines.add("")
  lines.add("{.push warning[UnusedImport]: off.}")
  lines.add("import treestand/parser_types")
  lines.add("import std/times")
  lines.add("import std/options")
  lines.add("import std/algorithm")
  lines.add("import std/strutils")
  lines.add("import std/unicode")
  lines.add("{.pop.}")
  
  # Add path hint for runtime helpers
  lines.add(&"{{.hint[Path]: on.}}")
  lines.add("")

  # Generate symbol enum
  lines.add(generateSymbolEnum(syntaxGrammar, lexicalGrammar, externalTokens))
  lines.add("")
  
  # Generate external scanner bindings if needed
  if externalTokens.len > 0:
    lines.add(generateExternalScannerBindings(grammarName, grammarPath, externalTokens))
    lines.add("")
  
  # Generate parse table
  lines.add(generateParseTableCode(parseTable, lexicalGrammar, externalTokens))
  lines.add("")
  
  # Generate lex table
  lines.add(generateLexTableCode(lexTable))
  lines.add("")
  
  # Generate lexer runtime
  lines.add(generateLexerRuntime(lexTable, lexicalGrammar, externalTokens, syntaxGrammar.extraSymbols))
  lines.add("")
  
  # Generate parser runtime  
  lines.add(generateParserRuntime(lexicalGrammar, syntaxGrammar.extraSymbols))
  
  # Public API
  lines.add(&"proc parse{grammarName.snakeToCamel()}*(input: string): ParseNode =")
  lines.add("  var parser = newParser(input)")
  lines.add("  return parser.parse()")
  lines.add("")
  
  # Include runtime helpers at the end (after ParseNode is defined)

  result = lines.join("\n")
