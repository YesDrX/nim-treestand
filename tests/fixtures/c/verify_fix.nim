
import parser
import treestand/parser_types
import std/strutils

echo "Inspecting LexState 26 (used by state 121)"
let state26 = lexStates[26]
echo "Accept Symbol: ", state26.acceptSymbol

var handlesX = false
for t in state26.transitions:
    echo "Transition: ", t.minChar, "-", t.maxChar, " -> ", t.nextState
    if 'x'.ord >= t.minChar and 'x'.ord <= t.maxChar:
        echo "  MATCHES 'x'!"
        handlesX = true

if not handlesX:
    echo "  NO TRANSITION FOR 'x' (", 'x'.ord, ")"

echo "---------------------------------------------------"

let input = "int x;"
echo "Parsing input: '", input, "'"
try:
  let tree = parseC(input)
  echo "Parse result kind: ", tree.symbol.kind
  
  if tree.symbol.kind == skError:
    echo "Parse FAILED: returned Error node"
    quit(1)
    
  echo "Parse SUCCESS!"
  # Recursively print the tree to verify structure
  proc printTree(node: ParseNode, depth: int = 0) =
    let indent = repeat("  ", depth)
    case node.symbol.kind
    of skError:
      echo indent, "Error!"
    of skTerminal:
      echo indent, "Token(", node.token.kind.terminalIndex, "): ", node.token.text
    of skNonTerminal:
      echo indent, "Rule(", node.symbol.nonTerminalIndex, ")"
      for child in node.children:
        printTree(child, depth + 1)
        
  printTree(tree)
  
except Exception as e:
  echo "Parse CRASHED: ", e.msg
  echo e.getStackTrace()
  quit(1)
