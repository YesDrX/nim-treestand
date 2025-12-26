import std/strutils
include parser

proc main() =
  # Input uses the external comment token '#'
  let input = "123 + 456 # This is a comment\n"
  
  echo "Input: ", input
  var parser = newParser(input)
  let tree = parser.parse()
  
  if tree != nil:
    echo "S-Expr: ", toSExpr(tree)
  else:
    echo "Parse failed"

when isMainModule:
  main()
