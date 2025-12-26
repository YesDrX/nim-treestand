import std/options
import treestand

tsGrammar "test":
  binary <- (left: expr) * op * ?(right: expr)
  expr   <- number
  op     <- token("=")
  number <- token(re"\d+")
  
  extras = token(re"\s+")

when isMainModule:
  let g = test()
  echo "Success! Generated grammar with ", g.variables.len, " variables"
  for v in g.variables:
    echo "  - ", v.name
