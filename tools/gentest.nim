
import treestand/cli/generate
import std/[os]

when isMainModule:
  let grammarPath = "tests/fixtures/yaml/grammar.js"
  let outputDir = "tests/fixtures/yaml"
  
  echo "Running generator for: ", grammarPath
  generateParser(grammarPath, outputDir)
