import unittest
import std/os
import std/osproc
import treestand
import std/strutils

# importGrammar currentSourcePath().parentDir() / "fixtures" / "yaml" / "grammar.js"

# suite "YAML Grammar":
#   test "Parse yaml":
#     let yamlStr = """
# ---
# project: treestand
# version: 1.0
# features: ["parser-generator", "grammar-compilation", "runtime-library"]
# meta:
#     author: yesdrx
#     active: true
#     score: 99.5
#     dependencies: null
# """
#     echo parseYaml(yamlStr)

suite "YAML Grammar Pipeline":
  
  test "Full Pipeline run with YAML Grammar":
    let testDir = currentSourcePath().parentDir()
    let fixtureDir = testDir / "fixtures" / "yaml"
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
    var externalTokens: seq[string] = @[]
    for rule in inputGrammar.externalTokens:
      var tokenName = ""
      if rule.kind == rkNamedSymbol:
        tokenName = rule.symbolName
      elif rule.kind == rkString:
        tokenName = rule.stringValue
      
      if tokenName != "":
        externalTokens.add(tokenName)
    
    echo "Extracted external tokens: ", externalTokens
    let parserCode = generateParser(
      "yaml",
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
      "# A reasonably complex YAML sample\n" &
      "let yamlStr = " & q3 & "\n" &
      "---\n" &
      "project: treestand\n" &
      "version: 1.0\n" &
      "features: [\"parser-generator\", \"grammar-compilation\", \"runtime-library\"]\n" &
      "meta:\n" &
      "    author: yesdrx\n" &
      "    active: true\n" &
      "    score: 99.5\n" &
      "    dependencies: null\n" &
      # "unicode: \"\\u00A9 2025\"\n" & # unicode not working?
      q3 & "\n\n" &
      """
echo "========================================"
echo "Input YAML:"
echo yamlStr
echo "========================================"

try:
  let tree = parseYaml(yamlStr)
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
    let compileCmd = "nim c -r -d:debug --hints:off --path:" & srcDir.quoteShell & " " & runnerPath.quoteShell
    echo "  Running: ", compileCmd
    let exitCode = execCmd(compileCmd)
    
    if exitCode != 0:
      echo "Runner failed with exit code ", exitCode
      fail()
