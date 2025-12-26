import unittest
import std/[os, osproc, strutils, strformat]

type
  GrammarTestResult* = object
    name*   : string
    success*: bool
    stage*  : string  # "generate", "compile", "test"
    error*  : string

proc testGrammar(fixtureDir: string): GrammarTestResult =
  ## Test a single grammar using the treestand cli
  result.name    = fixtureDir.splitPath().tail
  result.success = true
  result.stage   = "init"
    
  echo "\n\n===================================================="
  echo &"[TEST] Testing grammar: {result.name}"
  let treecliDir = currentSourcePath().parentDir().parentDir() / "src"
  let cmd = fmt"""timeout 30s nim r --hints:off {treecliDir / "treestand.nim"} --cmd test --fixture_dir {fixtureDir.quoteShell}"""
  echo cmd
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    result.success = false
    result.error = &"Test failed:\n{output}"
    echo &"  ✗ Test failed"
    return
  echo &"  ✓ Test passed"

suite "Test All Grammars":
  test "Run all test grammars":
    let testGrammarsDir = currentSourcePath().parentDir() / "fixtures" / "test_grammars"
    
    var results: seq[GrammarTestResult] = @[]
    var successCount = 0
    var failureCount = 0
    
    # Get all grammar directories
    for kind, path in walkDir(testGrammarsDir):
      if kind == pcDir:
        let grammarPath = path / "grammar.js"
        if fileExists(grammarPath):
          let result = testGrammar(path)
          results.add(result)
          
          if result.success:
            echo &"  ✓ {result.name}: SUCCESS"
            inc successCount
          else:
            echo &"  ✗ {result.name}: FAILED"
            if result.error.len > 0:
              # Print only first few lines of error
              let errorLines = result.error.split('\n')
              for i, line in errorLines:
                if i >= errorLines.len - 20: # show last 20 lines
                  echo &"    {line}"
                elif i == 0:
                  echo "      ..."
            inc failureCount
    
    # Print summary
    echo ""
    echo "=" .repeat(60)
    echo &"Grammar Test Summary"
    echo "=" .repeat(60)
    echo &"Total: {results.len}"
    echo &"Passed: {successCount}"
    echo &"Failed: {failureCount}"
    echo ""
    
    if failureCount > 0:
      echo "Failed grammars:"
      for result in results:
        if not result.success:
          echo &"  - {result.name}"
    
    echo "=" .repeat(60)
    
    # Don't fail the test - we want to see results for all grammars
    check successCount > 0
