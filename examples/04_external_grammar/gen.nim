import treestand
import std/os
import std/strutils

when isMainModule:
  generateParser(
    currentSourcePath().parentDir() / "grammar.js",
    currentSourcePath().parentDir()
  )
  echo "Parser generated to ", currentSourcePath().parentDir() / "parser.nim"