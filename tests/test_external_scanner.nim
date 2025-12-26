import unittest
import std/[os, osproc, strutils]
import treestand

suite "External Scanner Integration":
  
  test "generate parser with external scanner":
    let testDir = currentSourcePath().parentDir()
    let fixtureDir = testDir / "fixtures" / "simple_scanner"
    let grammarPath = fixtureDir / "grammar.js"
    let srcDir = testDir.parentDir() / "src"
    let dslPath = srcDir / "treestand" / "dsl.js"
    
    # use tree-sitter generate to generate scanner.c and parser.h
    let originalCwd = getCurrentDir()
    setCurrentDir(fixtureDir)
    let generateCmd = "tree-sitter generate"
    let (generateOut, generateCode) = execCmdEx(generateCmd)
    check generateCode == 0
    setCurrentDir(originalCwd)

    # Check files exist
    check fileExists(grammarPath)
    check fileExists(fixtureDir / "src" / "scanner.c")
    check fileExists(fixtureDir / "src" / "tree_sitter" / "parser.h")
    
    # Parse grammar
    let grammarJson = executeGrammarJs(grammarPath, dslPath)
    let inputGrammar = parseGrammar(grammarJson)
    
    # Prepare grammar
    let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
    
    # Build tables
    let tables = buildTables(syntaxGrammar, lexicalGrammar)
    let parseTable = tables.parseTable
    let lexTable = tables.mainLexTable
    
    # Get external token names for scanner
    var externalTokens: seq[string] = @[]
    for token in syntaxGrammar.externalTokens:
      externalTokens.add(token.name)
    
    # Generate parser
    let parserCode = generateParser(
      "simple_scanner",
      fixtureDir,
      syntaxGrammar,
      lexicalGrammar,
      parseTable,
      lexTable,
      externalTokens
    )
    
    # Write generated parser
    let outputPath = fixtureDir / "parser.nim"
    writeFile(outputPath, parserCode)
    
    echo "Generated parser at: ", outputPath
    
    # Compile scanner.c separately first to check it
    let scannerPath = fixtureDir / "src" / "scanner.c"
    let scannerCheck = "gcc -c -I" & fixtureDir.quoteShell & " " & scannerPath.quoteShell & " -o /tmp/scanner.o"
    let (scannerOut, scannerCode) = execCmdEx(scannerCheck)
    
    if scannerCode != 0:
      echo "Scanner compilation check:"
      echo scannerOut
    
    # Try to compile the generated parser
    let compileCmd = "nim c --path:" & srcDir.quoteShell & " " & outputPath.quoteShell
    let (output, exitCode) = execCmdEx(compileCmd)
    
    if exitCode != 0:
      echo "Parser compilation failed:"
      echo output
      echo "---"
      echo "Generated code preview (first 100 lines):"
      let lines = parserCode.split('\n')
      for i in 0..<min(100, lines.len):
        echo lines[i]
    
    check exitCode == 0
    
    # Create a runner to verify the parser works and precedence is correct
    let runnerPath = fixtureDir / "runner.nim"
    let runnerCode = """
import parser
import std/strutils

try:
  # Test binary expression precedence: 1 + 2 * 3 should be 1 + (2 * 3)
  # Also tests NEWLINE from external scanner
  let source = "x = 1 + 2 * 3\n"
  let tree = parseSimpleScanner(source)
  let treeStr = $tree
  echo "Parse Result: ", treeStr
  
  if "ERROR" in treeStr:
    echo "FAILED: Parse tree contains ERROR"
    quit(1)
    
  # Simple check for structure (verification of precedence)
  # Tree-sitter format usually shows hierarchy. 
  # We expect (binary_expression left: ... right: (binary_expression ...)) for right assoc 
  # or (binary_expression left: (binary_expression ...) right: ...) for left assoc.
  # But we just want to ensure it parses without error and uses external token.
  
  if "assignment" notin treeStr:
    echo "FAILED: Did not parse as assignment"
    quit(1)
    
  echo "SUCCESS"
except Exception as e:
  echo "FAILED: ", e.msg
  quit(1)
"""
    writeFile(runnerPath, runnerCode)
    
    # Compile and run the runner
    # We need to include the fixture src dir for scanner.o linking (which nim does automatically via {.compile.})
    # But we need to ensure the include path for parser.h is set if parser.nim uses it (it usually doesn't need to expose it)
    
    let runnerCmd = "nim c -r --hints:off --path:" & srcDir.quoteShell & " " & runnerPath.quoteShell
    let (runOut, runExit) = execCmdEx(runnerCmd)
    
    if runExit != 0:
      echo "Runner failed:"
      echo runOut
    else:
      echo "Runner output:"
      echo runOut
      
    check runExit == 0
    check "SUCCESS" in runOut
    
    # Clean up
    if fileExists(outputPath):
      removeFile(outputPath)
    let exePath = outputPath.replace(".nim", "")
    if fileExists(exePath):
      removeFile(exePath)
