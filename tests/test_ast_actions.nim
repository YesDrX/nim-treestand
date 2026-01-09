import std/[unittest, strutils, tables, hashes]
import treestand

# ==============================================================================
# Custom AST Definition
# ==============================================================================

type
  ExprKind = enum
    ekInt, ekBinary

  Expr = ref object
    case kind: ExprKind
    of ekInt:
      intVal: int
    of ekBinary:
      op: string
      left, right: Expr

proc `$`(e: Expr): string =
  case e.kind
  of ekInt: $e.intVal
  of ekBinary: "(" & $e.left & " " & e.op & " " & $e.right & ")"

# ==============================================================================
# AST Builder using Table-based approach
# ==============================================================================

# ParseNode needs a hash proc to be used as Table key
proc hash(node: ParseNode): Hash =
  hash(cast[int](node))

type
  AstBuilder = object
    astMap: Table[ParseNode, Expr]
    rootAst: Expr  # The final AST result

tsGrammar "math", userdata: AstBuilder:
  # Main entry point
  program     <- expression:
    # Propagate final result AND store as root
    let exprNode = node.children[0]
    if exprNode in userdata.astMap:
      let ast = userdata.astMap[exprNode]
      userdata.astMap[node] = ast
      userdata.rootAst = ast  # Store root for easy retrieval!
  
  # Expression can be binary or primary
  expression  <- binary_op | primary:
    # Propagate from child
    let childNode = node.children[0]
    if childNode in userdata.astMap:
      userdata.astMap[node] = userdata.astMap[childNode]
  
  # Binary operations with precedence
  binary_op   <- (expression * op * expression) ^ 1:
    # Children: expression (0), op (1), expression (2)
    let leftNode = node.children[0]
    let rightNode = node.children[2]
    
    # Get AST nodes from children (they should have been built already)
    if leftNode in userdata.astMap and rightNode in userdata.astMap:
      let left = userdata.astMap[leftNode]
      let right = userdata.astMap[rightNode]
      let opText = node.children[1].text
      
      userdata.astMap[node] = Expr(kind: ekBinary, op: opText, left: left, right: right)
  
  # Primary expressions  
  primary     <- number | parens:
    # Propagate from child
    let childNode = node.children[0]
    if childNode in userdata.astMap:
      userdata.astMap[node] = userdata.astMap[childNode]
  
  parens      <- lparen * expression * rparen:
    # Extract the expression from between parentheses
    # Children: lparen (0), expression (1), rparen (2)
    let exprNode = node.children[1]
    if exprNode in userdata.astMap:
      userdata.astMap[node] = userdata.astMap[exprNode]
  
  # Terminals with actions
  number      <- token(re"(-?\d+)"):
    let val = parseInt(node.text)
    userdata.astMap[node] = Expr(kind: ekInt, intVal: val)
  
  # Lexical tokens (no actions needed for delimiters)
  op          <- token(re"[+\-*/]")
  lparen      <- token("(")
  rparen      <- token(")")
  
  # Skip whitespace
  extras      = token(re"\s+")

# ==============================================================================
# Tests
# ==============================================================================

test "Build AST using actions - simple":
  var builder = AstBuilder()
  builder.astMap = initTable[ParseNode, Expr]()
  
  let input = "1 + 2"
  
  if matchMath(input, builder):
    echo "Parsed successfully!"
    echo "Root AST: ", builder
    
    # Now we can directly access the root AST!
    check builder.rootAst != nil
    check builder.rootAst.kind == ekBinary
    check builder.rootAst.op == "+"
    check builder.rootAst.left.intVal == 1
    check builder.rootAst.right.intVal == 2
  else:
    echo "Parse failed!"
    fail()

test "Build AST using actions - complex":
  var builder = AstBuilder()
  builder.astMap = initTable[ParseNode, Expr]()
  
  let input = "1 + 2 * 3"
  
  if matchMath(input, builder):
    echo "Parsed: ", input
    echo "Root AST: ", builder.rootAst
    
    # Directly access root AST
    check builder.rootAst != nil
    check builder.rootAst.kind == ekBinary
    check builder.rootAst.op == "*"
    # Due to left-associativity: ((1 + 2) * 3)
    check builder.rootAst.left.kind == ekBinary
    check builder.rootAst.left.op == "+"
    check builder.rootAst.right.intVal == 3
  else:
    echo "Parse failed!"
    fail()
