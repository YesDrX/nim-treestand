import std/[os, macros, options]
import treestand/cli/[generate, test]
import treestand/[
  js_exec, 
  parse_grammar, 
  prepare_grammar, 
  build_tables, 
  codegen,
  grammar,
  dsl,
  parser_types,
  query,
  nfa,
  ts_macros
]
export 
  js_exec, 
  parse_grammar, 
  prepare_grammar, 
  build_tables, 
  codegen,
  grammar,
  dsl,
  parser_types,
  query,
  nfa,
  generate,
  test,
  macros,
  ts_macros,
  options

proc main(
  cmd         : string,
  grammarPath : string = "",
  dslPath     : string = "",
  outputDir   : string = "",
  parserName  : string = "",
  fixtureDir  : string = ""
) {.used.} =
  if cmd notin ["generate", "test"]:
    echo "Error: Unknown command '", cmd, "'"
    quit(1)
  
  if cmd == "generate" and grammarPath.len == 0:
    echo "Error: No grammar file specified"
    quit(1)
  
  if cmd == "test" and fixtureDir.len == 0:
    echo "Error: No fixture directory specified"
    quit(1)

  if cmd == "generate":
    generateParser(grammarPath, outputDir, dslPath, parserName)
  elif cmd == "test":
    testCommand(fixtureDir)

proc generateParser*(
  grammar        : InputGrammar,
  outputFilename : string,
  externalTokens : seq[string] = @[],
  grammarPath    : string = "",
) =
  ## Generate a complete parser from an `InputGrammar` and write it to a file.
  ##
  ## This is a high-level procedure that takes a parsed grammar and produces a complete,
  ## ready-to-use Nim parser module. It orchestrates the entire code generation pipeline:
  ## grammar preparation → table generation → code generation → file writing.
  ##
  ## **Parameters:**
  ## - `grammar`: The input grammar object (obtained from `parseGrammar`, `parseGrammarJson`, or `parseGrammarJs`)
  ## - `outputFilename`: Path where the generated `parser.nim` file will be written
  ## - `externalTokens`: List of external scanner token names (default: empty)
  ## - `grammarPath`: Directory containing external scanner source files like `scanner.c` (default: empty)
  ##
  ## **Behavior:**
  ## 1. Prepares the grammar by separating syntax and lexical rules
  ## 2. Builds LR parsing tables and lexical state machines
  ## 3. Generates complete Nim parser code with runtime support
  ## 4. Creates output directory if it doesn't exist
  ## 5. Writes the final parser code to `outputFilename`
  ##
  ## If `grammarPath` is provided and contains a `scanner.c` file, external scanner
  ## bindings will be generated automatically.
  ##
  ## **Example:**
  ## ```nim
  ## import treestand
  ##
  ## # Parse grammar from grammar.js
  ## let grammar = parseGrammarJs("grammar.js")
  ##
  ## # Generate parser
  ## generateParser(
  ##   grammar = grammar,
  ##   outputFilename = "generated/parser.nim",
  ##   externalTokens = @["comment", "string"],
  ##   grammarPath = "."
  ## )
  ## ```
  let (syntaxGrammar, lexicalGrammar) = prepareGrammar(grammar)
  let tables = buildTables(syntaxGrammar, lexicalGrammar)
  let parseTable = tables.parseTable
  let lexTable = tables.mainLexTable

  let code = generateParser(
    grammar.name,
    grammarPath = grammarPath,
    syntaxGrammar = syntaxGrammar,
    lexicalGrammar = lexicalGrammar,
    parseTable = parseTable,
    lexTable = lexTable,
    externalTokens = externalTokens
  )

  if not dirExists(outputFilename.parentDir()):
    createDir(outputFilename.parentDir())
  
  writeFile(outputFilename, code)

when isMainModule:
  import cligen
  cligen.dispatch(main)
