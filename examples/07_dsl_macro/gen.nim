import std/[options, tables]
import treestand

# ==============================================================================
# Example: Grammar with Embedded Actions
# ==============================================================================

type
  Stats = object
    exprCount: int
    binaryOps: int

tsGrammar "calc", userdata: Stats:
  program <- +stmt:
    echo "Parsed ", userdata.exprCount, " expressions with ", userdata.binaryOps, " binary operations"
  
  stmt <- expression * semi:
    userdata.exprCount += 1
  
  expression <- number | binary | parens
  
  binary <- (expression * op * expression) ^ 1:
    userdata.binaryOps += 1
    echo "Found binary op: ", node.child(1).text
  
  parens <- lparen * expression * rparen:
    echo "Found parenthesized expression"
  
  number <- token(re"\d+")
  op <- token(re"[+\-*/]")
  semi <- token(";")
  lparen <- token("(")
  rparen <- token(")")
  
  extras = token(re"\s+")

when isMainModule:
  echo "=== Example: Grammar with Actions ==="
  var stats = Stats()
  let input = "1 + 2; 3 * 4; (5 + 6) * 7;"
  echo "Input: ", input
  echo ""
  
  if matchCalc(input, stats):
    echo ""
    echo "Success!"
  else:
    echo "Parse failed"
  
  echo "\n=== Complete ==="
