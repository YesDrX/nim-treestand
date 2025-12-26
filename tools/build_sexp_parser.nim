import treestand
import std/[os, strutils, strformat, osproc]

when isMainModule:
  let currentSourceDir = currentSourcePath().parentDir()
  let jsPath = currentSourceDir / "sexp_grammar" / "grammar.js"
  if not fileExists(jsPath):
    quit("Error: grammar.js not found at " & jsPath)
  
  let cmd = &"""nim r -f {currentSourceDir.parentDir() / "src" / "treestand.nim"} --cmd generate --grammar_path {jsPath} --output_dir {currentSourceDir}"""
  let exitCode = execCmd(cmd)
  if exitCode != 0:
    quit("Error: failed to generate sexp parser")

  writeFile(
    currentSourceDir.parentDir() / "src" / "treestand" / "sexp_parser.nim",
    readFile(currentSourceDir / "parser.nim").replace(
      "import treestand/parser_types", "import parser_types"
    ).replace(
      "import std/algorithm", ""
    ).replace(
      "import std/unicode", ""
    ).replace(
      "include treestand/parser_runtime", "include parser_runtime"
    )
  )
  
  removeFile(currentSourceDir / "parser.nim")
  