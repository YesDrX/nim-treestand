import unittest, os, osproc

suite "Compile Examples":
    test "Compile Examples":
        let examplesDir = currentSourcePath().parentDir().parentDir() / "examples"
        for example_dir in examplesDir.walkDir():
            if example_dir.kind == pcFile: continue
            let genNim = example_dir.path / "gen.nim"
            let mainNim = example_dir.path / "main.nim"
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