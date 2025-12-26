import treestand
import std/strutils

# Include generated parser
include parser

proc lookupName(s: Symbol): string =
  if s.kind == skNonTerminal:
    return $NonTerminalSymbol(s.nonTerminalIndex)
  else:
    var name = TerminalSymbol(s.terminalIndex)
    return $name

proc main() =
  let input = "1 + 2 + 3"
  var parser = newParser(input)
  let tree = parser.parse()
  
  if tree == nil:
    echo "Parse failed"
    return

  echo "S-Expr: ", toSExpr(tree)
  
  echo "\nQuery Demo:"
  # Match binary operations containing a number
  # Note: The grammar structure is (expression (binary_op ...)) or (expression (number))
  # Query: Find all numbers
  let qSource = """
    (expression (number) @my_val)
  """
  
  try:
    let q = newQuery(qSource)
    if q.patterns.len == 0:
      echo "No patterns parsed. Check query syntax."
    
    echo "Executing query..."
    var count = 0
    
    # Iterate over matches
    for m in exec(q, tree, lookupName):
      count += 1
      echo "Match ", count, ":"
      for cap in m.captures:
        echo "  Capture: ", cap.name, " -> ", cap.node.token.text
        
    echo "Done. Matches: ", count
  except CatchableError as e:
    echo "Query Error: ", e.msg
