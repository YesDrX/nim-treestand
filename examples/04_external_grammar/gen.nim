import treestand
import std/os

when isMainModule:
  generateParser(
    currentSourcePath().parentDir() / "grammar.js",
    currentSourcePath().parentDir()
  )
  echo "Parser generated to ", currentSourcePath().parentDir() / "parser.nim"