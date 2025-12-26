import std/[os, strformat, osproc, strutils]
import ../[corpus]

proc prepareExternalScanner(fixtureDir: string, expectError: bool): tuple[success: bool, output: string] =
  var treeSitterTool = findExe("tree-sitter")

  if treeSitterTool.len == 0:
    treeSitterTool = "~/.cargo/bin/tree-sitter".expandTilde()
    
  if not fileExists(treeSitterTool):
    echo "[TEST] Warning: tree-sitter not found at ", treeSitterTool
    echo "[TEST] Proceeding without regenerating headers"
    return (true, "Tree-sitter not found")
  
  # Run tree-sitter generate in the fixture directory
  let originalCwd = getCurrentDir()
  setCurrentDir(fixtureDir)
  let cmd = &"{treeSitterTool.quoteShell} generate" # remove 2>&1 to make windows happy
  echo "[TEST] Running: ", cmd
  let (output, exitCode) = execCmdEx(cmd)
  setCurrentDir(originalCwd)
  
  if not expectError and exitCode != 0:
    echo "[TEST] Warning: tree-sitter generate failed."
    return (false, output)

  if expectError and exitCode == 0:
    echo "[TEST] Warning: tree-sitter generate succeeded but expected failure."
    return (false, output)
  
  return (true, output)

proc testCommand*(fixtureDir: string) =
  ## Test a fixture folder (corpus.txt for positive tests, expected_error.txt for negative)
  echo "Testing fixture: ", fixtureDir
  
  if not dirExists(fixtureDir):
    echo "Error: Fixture directory not found: ", fixtureDir
    quit(1)
  
  let corpusPath = fixtureDir / "corpus.txt"
  let errorPath = fixtureDir / "expected_error.txt"
  let grammarPath = fixtureDir / "grammar.js"
  let expectParserGenerationFailure = fileExists(errorPath)
  
  if not fileExists(grammarPath):
    echo "[TEST Error]: grammar.js not found in fixture"
    quit(1)
    
  if not fileExists(corpusPath) and not fileExists(errorPath):
    echo "[TEST Error]: No corpus.txt or expected_error.txt found in fixture"
    quit(1)
  
  echo "[TEST] Preparing external scanner ..."
  let (scannerSuccess, scannerOutput) = prepareExternalScanner(fixtureDir, expectError = expectParserGenerationFailure)
  if not scannerSuccess:
    echo "[TEST Error]: Failed to prepare external scanner"
    quit(1)
  
  # Generate parser
  let parserPath = fixtureDir / "parser.nim"
  echo "[TEST] Generating parser to: ", parserPath
  if fileExists(parserPath):
    echo "[TEST] Removing existing parser.nim"
    removeFile(parserPath)
  
  let treestand_filename = currentSourcePath().parentDir().parentDir().parentDir() / "treestand.nim"

  when defined(debug):
    let generateCmd = &"""nim r -d:debug --hints:off {treestand_filename.quoteShell} --cmd generate --grammar_path {grammarPath.quoteShell} --output_dir {fixtureDir.quoteShell}"""
  else:
    let generateCmd = &"""nim r --hints:off {treestand_filename.quoteShell} --cmd generate --grammar_path {grammarPath.quoteShell} --output_dir {fixtureDir.quoteShell}"""
  
  echo "[Treestand] Running: ", generateCmd
  let (genOutput, genExit) = execCmdEx(generateCmd)
  
  # Handle negative tests (expected to fail)
  if expectParserGenerationFailure:
    if genExit != 0:
      echo "[TEST] ✓ Parser generation failed as expected (negative test passed)"
      return  # Success for negative test
    else:
      echo "---------BEGIN TREE-SITTER OUTPUT---------"
      echo scannerOutput
      echo "---------END TREE-SITTER OUTPUT---------"
      echo "---------BEGIN TREESTAND OUTPUT---------"
      echo genOutput
      echo "---------END TREESTAND OUTPUT---------"
      echo "[TEST Error]: Expected parser generation to fail but it succeeded"
      quit(1)
  
  # Handle positive tests (expected to succeed)
  if genExit != 0:
    echo "[TEST Error]: Error generating parser:"
    echo genOutput
    quit(1)
  
  if not fileExists(parserPath):
    echo "[TEST Error]: Parser not generated at ", parserPath
    quit(1)
  
  # Run corpus tests with the old testCommand logic
  echo "[TEST] Testing parser with corpus: ", corpusPath
  
  # Read and parse corpus
  let corpusContent = readFile(corpusPath)
  let tests = parseCorpus(corpusContent)
  
  if tests.len == 0:
    echo "[TEST Warning]: No tests found in corpus"
    return
  
  echo "[TEST] Found ", tests.len, " test(s) in corpus"
  
  # Extract grammar name from parser file
  let parserContent = readFile(parserPath)
  var grammarName = "unknown"
  for line in parserContent.splitLines():
    if line.contains("proc parse") and line.contains("*(input: string"):
      let parts = line.split("parse")
      if parts.len > 1:
        let nameEnd = parts[1].find('(')
        if nameEnd > 0:
          grammarName = parts[1][0..<nameEnd].replace("*", "")
          break
  
  echo "[TEST] Detected parser function: parse", grammarName
  
  # Generate test runner
  let runnerDir = fixtureDir
  
  var runnerCode = "import parser\n\n"
  runnerCode.add("var passed = 0\nvar failed = 0\n\n")
  
  for i, test in tests:
    let escapedInput = test.input.multiReplace([("\\", "\\\\"), ("\"", "\\\""), ("\n", "\\n")])
    let escapedName = test.name.multiReplace([("\"", "\\\"")])
    # let escapedExpected = test.expected.multiReplace([("\\", "\\\\"), ("\"", "\\\""), ("\n", "\\n")])
    let expectsError = test.expected.contains("(ERROR") or test.expected.contains("(MISSING")
    
    runnerCode.add(&"""
echo "Test {i+1}/{tests.len}: {escapedName}"
let input{i} = "{escapedInput}"
let expectsError{i} = {expectsError}

try:
  let tree{i} = parse{grammarName}(input{i})
  echo "  Result: ", tree{i}
  inc passed
except Exception as e:
  if expectsError{i}:
    echo "  Passed (Expected Parse Error): ", e.msg
    inc passed
  else:
    echo "  FAILED: ", e.msg
    inc failed
""")
  
  runnerCode.add("\necho \"\\nResults: \", passed, \" passed, \", failed, \" failed\"\n")
  runnerCode.add("if failed > 0: quit(1)\n")
  
  let runnerPath = runnerDir / "runner.nim"
  writeFile(runnerPath, runnerCode)
  
  # Compile and run
  # let parserDir = parserPath.parentDir()
  let treeStandSrc = currentSourcePath().parentDir().parentDir().parentDir()
  let includeDir = treeStandSrc / ".." / ".." / "lib" / "include"
  let grammarSrcInclude = fixtureDir / "src"
  
  var passC = ""
  passC = " --passC:\"-I" & includeDir & "\""
  passC = " --passC:\"-I" & grammarSrcInclude & "\""
  
  when defined(debug):
    let compileCmd = "nim r -d:debug --hints:off" & passC & " --path:" & treestandSrc.quoteShell & " " & runnerPath.quoteShell
  else:
    let compileCmd = "nim r --hints:off" & passC & " --path:" & treestandSrc.quoteShell & " " & runnerPath.quoteShell
  
  echo "[TEST] Running tests:"
  echo compileCmd

  let exitCode = execCmd(compileCmd)
  
  if exitCode != 0:
    echo "[TEST Error]: Tests failed with exit code: ", exitCode
    quit(1)
  else:
    echo "[TEST] ✓ All tests passed!"
