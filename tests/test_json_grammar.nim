import unittest
import std/[os, osproc]
import treestand

suite "JSON Grammar Pipeline":
  
  test "Full Pipeline run with JSON Grammar":
    let testDir = currentSourcePath().parentDir()
    let fixtureDir = testDir / "fixtures" / "json"
    let grammarPath = fixtureDir / "grammar.js"
    let srcDir = testDir.parentDir() / "src"
    let dslPath = srcDir / "treestand" / "dsl.js"
    
    # Ensure fixture dir exists (it should)
    if not dirExists(fixtureDir):
      createDir(fixtureDir)
      
    echo "1. Parsing grammar form ", grammarPath
    let grammarJson = executeGrammarJs(grammarPath, dslPath)
    let inputGrammar = parseGrammar(grammarJson)
    
    echo "2. Preparing grammar"
    let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
    
    echo "3. Building tables"
    let tables = buildTables(syntaxGrammar, lexicalGrammar)
    
    echo "4. Generating parser"
    let externalTokens: seq[string] = @[] # JSON has none
    let parserCode = generateParser(
      "json",
      fixtureDir,
      syntaxGrammar,
      lexicalGrammar,
      tables.parseTable,
      tables.mainLexTable,
      externalTokens
    )
    
    let parserPath = fixtureDir / "parser.nim"
    writeFile(parserPath, parserCode)
    echo "   Saved to ", parserPath
    
    echo "5. Creating Test Runner"
    let runnerPath = fixtureDir / "runner.nim"
    
    # Construct triple quote string correctly
    let q3 = "\"\"\""
    
    let runnerCode = "import parser\nimport std/strutils\n\n" &
      "# A reasonably complex JSON sample\n" &
      "let jsonStr = " & q3 & "\n" &
      "{\n" &
      "  \"project\": \"treestand\",\n" &
      "  \"version\": 1.0,\n" &
      "  \"features\": [\n" &
      "    \"parser-generator\", \n" &
      "    \"grammar-compilation\",\n" &
      "    \"runtime-library\"\n" &
      "  ],\n" &
      "  \"meta\": {\n" &
      "    \"author\": \"yesdrx\",\n" &
      "    \"active\": true,\n" &
      "    \"score\": 99.5,\n" &
      "    \"dependencies\": null\n" &
      "  },\n" &
      "  \"unicode\": \"\\u00A9 2025\"\n" &
      "}\n" &
      q3 & "\n\n" &
      """
echo "========================================"
echo "Input JSON:"
echo jsonStr
echo "========================================"

try:
  let tree = parseJson(jsonStr)
  echo "Parse Tree:"
  echo tree
  echo "========================================"
  echo "Success!"
except Exception as e:
  echo "CRITICAL FAILURE: ", e.msg
  # Print stack trace if possible?
  quit(1)
"""
    writeFile(runnerPath, runnerCode)
    
    echo "6. Running Verification"
    # Compile and run, capturing output
    let compileCmd = "nim c -r --hints:off --path:" & srcDir.quoteShell & " " & runnerPath.quoteShell
    let (output, exitCode) = execCmdEx(compileCmd)
    
    echo output
    
    if exitCode != 0:
      echo "Runner failed with exit code ", exitCode
      fail()
