
import treestand/cli/generate
import std/[os]

when isMainModule:
  let grammarPath = "tools/sexp_grammar/grammar.js"
  let outputDir = "tools/sexp_grammar"
  
  echo "Running generator for: ", grammarPath
  generateParser(grammarPath, outputDir)
