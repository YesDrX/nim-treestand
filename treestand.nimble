# Package

version       = "0.4.0"
author        = "Treestand Contributors"
description   = "Tree-sitter parser generator for Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["treestand"]
installExt    = @["nim", "js"]
installDirs   = @["src"]

# Dependencies

requires "nim >= 2.0.0"
requires "cligen"

task build, "Build the library and CLI":
  exec "nim c -d:release src/treestand.nim"

