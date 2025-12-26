import unittest, os, osproc, strformat

suite "Compile Examples":
    test "Compile Examples":
        let rootPath = currentSourcePath().parentDir().parentDir()
        let examplesDir = rootPath / "examples"
        for example_dir in examplesDir.walkDir():
            if example_dir.kind == pcFile: continue

            let grammarJs = example_dir.path / "grammar.js"
            let genNim = example_dir.path / "gen.nim"
            let mainNim = example_dir.path / "main.nim"
            let parserNim = example_dir.path / "parser.nim"

            if fileExists(grammarJs):
                # use tree-sitter to generate src/tree-sitter/*.h if needed
                setCurrentDir(example_dir.path)
                let cmd = "tree-sitter generate"
                echo "Running: " & cmd
                let exitCode = execCmd(cmd)
                check exitCode == 0
            
            if fileExists(genNim):
                let cmd = "nim r -f --hints:off " & $genNim
                echo "Running: " & cmd
                let exitCode = execCmd(cmd)
                check exitCode == 0
            else:
                echo "No gen.nim found in " & $example_dir
            if fileExists(mainNim):
                let cmd = "nim c -f --hints:off --path:" & example_dir.path & " " & $mainNim
                echo "Running: " & cmd
                let exitCode = execCmd(cmd)
                check exitCode == 0
            else:
                echo "No main.nim found in " & $example_dir
            if fileExists(parserNim):
                let cmd = "nim c -f --hints:off " & $parserNim
                echo "Running: " & cmd
                let exitCode = execCmd(cmd)
                check exitCode == 0
            else:
                echo "No parser.nim found in " & $example_dir
