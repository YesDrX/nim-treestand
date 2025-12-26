import unittest, std/strutils, std/options, std/os, std/osproc
import treestand
import treestand/nfa

suite "Code Generation":
  
  test "generateSymbolEnum - basic structure":
    let lexGrammar = LexicalGrammar(variables: @[], nfa: Nfa())
    let syntaxGrammar = SyntaxGrammar(
      variables: @[],
      extraSymbols: @[],
      expectedConflicts: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(GrammarSymbol),
      precedenceOrderings: @[],
      reservedWordSets: @[]
    )
    
    let code = generateSymbolEnum(syntaxGrammar, lexGrammar, @[])
    
    # Just verify it generates some code with enum
    check code.len > 0
    # check code.contains("Symbol* = object")
    check code.contains("TerminalSymbol")
    check code.contains("tsEOF")
    
  test "generateExternalScannerBindings - with tokens":
    createDir("/tmp/my_lang")
    writeFile("/tmp/my_lang/scanner.c", "")
    let code = generateExternalScannerBindings("my_lang", "/tmp/my_lang", @["indent", "dedent"])
    
    # Verify key components
    check code.contains("{.compile:")
    check code.contains("scanner.c")
    check code.contains("TSLexer")
    check code.contains("scanner_create")
    check code.contains("scanner_scan")
    
    removeFile("/tmp/my_lang/scanner.c")
    removeDir("/tmp/my_lang")
    
  test "generateExternalScannerBindings - empty":
    let code = generateExternalScannerBindings("test", "/tmp", @[])
    check code == ""
    
  test "generateParseTableCode - basic":
    let lexGrammar = LexicalGrammar(variables: @[], nfa: Nfa())
    let parseTable = BuildParseTable(
      entries: @[
        BuildParseTableEntry(
          actionMap: @[
            (sym: GrammarSymbol(kind: stNonTerminal, index: 0), action: BuildParseAction(kind: bpakAccept))
          ],
          gotoMap: @[]
        )
      ],
      productionInfos: @[],
      externalSymbols: @[]
    )
    
    let code = generateParseTableCode(parseTable, lexGrammar, @[])

    check code.contains("parseTableActions")
  
  test "generateParser - compilation test":
    # Create a minimal but complete grammar
    let lexGrammar = LexicalGrammar(
      variables: @[
        LexicalVariable(
          name: "number",
          kind: vtNamed,
          implicitPrecedence: 0,
          startState: 0,
          rule: Rule(kind: rkPattern, patternValue: "[0-9]+")
        )
      ],
      nfa: Nfa()
    )
    
    let syntaxGrammar = SyntaxGrammar(
      variables: @[
        SyntaxVariable(
          name: "expression", 
          kind: vtNamed, 
          productions: @[]
        )
      ],
      extraSymbols: @[],
      expectedConflicts: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(GrammarSymbol),
      precedenceOrderings: @[],
      reservedWordSets: @[]
    )
    
    let parseTable = BuildParseTable(
      entries: @[
        BuildParseTableEntry(
          actionMap: @[(sym: GrammarSymbol(kind: stNonTerminal, index: 0), action: BuildParseAction(kind: bpakAccept))],
          gotoMap: @[]
        )
      ],
      productionInfos: @[],
      externalSymbols: @[]
    )
    
    let lexTable = BuildLexTable(states: @[])
    
    # Generate complete parser code
    let generatedCode = generateParser(
      "test_parser", 
      "/tmp/test_parser",
      syntaxGrammar, 
      lexGrammar,
      parseTable, 
      lexTable, 
      @[]
    )
    
    # Write to temp file
    let tempDir = getTempDir()
    let tempFile = tempDir / "test_generated_parser.nim"
    
    writeFile(tempFile, generatedCode)
    
    # Try to compile it
    let path = "--path:" & currentSourcePath().parentDir().parentDir() / "src"
    let cmd = "nim c --hints:off " & path & " " & tempFile
    echo "[TEST] Running: " & cmd
    let (output, exitCode) = execCmdEx(cmd)
    
    # Clean up
    if fileExists(tempFile):
      removeFile(tempFile)
    
    let exeFileName = tempFile.replace(".nim", "")
    if fileExists(exeFileName):
      removeFile(exeFileName)
    
    # Check compilation succeeded
    if exitCode != 0:
      echo "Generated code failed to compile. Output:"
      echo output
    check exitCode == 0
