import treestand
import std/strutils

# Include the generated parser source file.
# In a larger project, you might compile this as a separate module and import it.
include parser

proc main() =
  # Define the input string to parse.
  let input = "1 + 2 * 3"
  
  # Initialize a new parser with the input string.
  # The parser maintains internal state including the lexer and parse stack.
  var parser = newParser(input)
  
  # Run the parser.
  # This returns the root ParseNode of the resulting derivation tree.
  # If parsing fails, it may return an error node or nil depending on recovery strategy.
  let tree = parser.parse()
  
  echo "Input: ", input
  echo "S-Expression:"
  if tree != nil:
    # `toSExpr` converts the parse tree into a Lisp-like string representation
    # which is standard for tree-sitter compatible parsers.
    echo toSExpr(tree)
  else:
    echo "Parse failed"

  # Example of traversing the parse tree manually.
  echo "\nTree Traversal:"
  proc walk(n: ParseNode, depth: int) =
    if n == nil: return
    let indent = repeat("  ", depth)
    
    # Access node information using runtime accessors:
    # `kind(n)`: Returns the string name of the node type (e.g. "binary_op", "number").
    # `n.startPos`, `n.endPos`: Byte offsets in the source string.
    echo indent, kind(n), " [", n.startPos, "..", n.endPos, "]"
    
    # Iterate over children nodes.
    # `children(n)` returns a sequence of child nodes.
    for c in children(n):
      walk(c, depth + 1)
      
  walk(tree, 0)

when isMainModule:
  main()
