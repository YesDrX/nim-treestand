import std/[ os]
import ../[js_exec, parse_grammar, prepare_grammar, build_tables, codegen]
import common

proc generateParser*(grammarPath: string, outputDir: string, dslPath: string = "", name: string = "") =
  ## Generate a parser from a `grammar.js` file (CLI-oriented interface).
  ##
  ## This procedure provides a complete end-to-end parser generation workflow, from
  ## a Tree-sitter `grammar.js` file to a ready-to-use Nim parser module. It handles
  ## all intermediate steps including DSL discovery, JavaScript execution, grammar
  ## parsing, table generation, and code generation.
  ##
  ## **Parameters:**
  ## - `grammarPath`: Path to the `grammar.js` file to process
  ## - `outputDir`: Directory where `parser.nim` will be written
  ## - `dslPath`: Custom path to `dsl.js` (optional; auto-discovered if not provided)
  ## - `name`: Custom parser name (optional; uses grammar name if not provided)
  ##
  ## **Workflow:**
  ## 1. **DSL Discovery**: Locates `dsl.js` (either from `dslPath` or auto-discovered)
  ## 2. **JS Execution**: Runs `grammar.js` with a JavaScript runtime (Bun → Node → Deno)
  ## 3. **Grammar Parsing**: Converts the JSON output to `InputGrammar`
  ## 4. **Grammar Preparation**: Separates syntax and lexical rules
  ## 5. **Table Building**: Generates LR parse tables and lexical automata
  ## 6. **Code Generation**: Creates complete Nim parser with runtime
  ## 7. **File Output**: Writes `parser.nim` to `outputDir`
  ##
  ## **External Scanner Support:**
  ## If a `scanner.c` file exists in the same directory as `grammar.js`, it will be
  ## automatically detected and bindings will be generated for external tokens.
  ##
  ## **Example:**
  ## ```nim
  ## import treestand
  ##
  ## # Generate parser from grammar.js
  ## generateParser(
  ##   grammarPath = "grammars/tree-sitter-json/grammar.js",
  ##   outputDir = "parsers/json"
  ## )
  ## # Creates: parsers/json/parser.nim
  ## ```
  ##
  ## **See Also:**
  ## - `parseGrammarJs` for parsing `grammar.js` files
  ## - `generateParser(InputGrammar, ...)` for programmatic generation
  echo "[Treestand] Generating parser from: ", grammarPath
  
  # 1. Find DSL
  let actualDslPath = if dslPath.len > 0: dslPath else: findDslJs()
  echo "[Treestand] Using dsl.js: ", actualDslPath
  
  # 2. Execute grammar.js to get JSON
  echo "[Treestand] Parsing grammar..."
  let absGrammarPath = expandFilename(grammarPath)
  let grammarJson = executeGrammarJs(absGrammarPath, actualDslPath)
  let inputGrammar = parseGrammar(grammarJson)
  
  # 3. Prepare grammar
  echo "[Treestand] Preparing grammar ..."
  let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
  
  # 4. Build tables
  echo "[Treestand] Building tables ..."
  let tables = buildTables(syntaxGrammar, lexicalGrammar)
  
  # 5. Generate parser
  echo "[Treestand] Generating parser ..."
  let parserName = if name.len > 0: name else: inputGrammar.name
  
  # Extract external tokens
  var externalTokens: seq[string] = @[]
  for ext in syntaxGrammar.externalTokens:
    externalTokens.add(ext.name)
  
  let parserCode = generateParser(
    parserName,
    absGrammarPath.parentDir(),  # Pass grammar directory so codegen can find scanner.c
    syntaxGrammar,
    lexicalGrammar,
    tables.parseTable,
    tables.mainLexTable,
    externalTokens
  )
  
  # 6. Write output
  if outputDir.len == 0:
    echo "# GENERATED PARSER.NIM"
    echo parserCode
  else:
    createDir(outputDir)
    let parserPath = outputDir / "parser.nim"
    writeFile(parserPath, parserCode)
    echo "[Treestand] ✓ Generated parser: ", parserPath
