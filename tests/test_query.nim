import treestand/query
import treestand/parser_types
import std/unittest
import std/options

# Helpers to build test tree
# We simplify: we assume `token` can hold name for dummy nodes in this test environment.

var nodeNameMap: seq[string] = @[]

proc makeNode(name: string, kids: varargs[ParseNode]): ParseNode =
  # Register name
  if name notin nodeNameMap: nodeNameMap.add(name)
  let idx = nodeNameMap.find(name)
  ParseNode(
    symbol: nonTerminal(idx),
    children: @kids
  )

proc makeAtom(text: string): ParseNode =
  ParseNode(
    symbol: terminal(0),
    token: Token(text: text)
  )

proc lookupName(s: Symbol): string =
  if s.kind == skNonTerminal:
    if s.nonTerminalIndex < nodeNameMap.len:
      return nodeNameMap[s.nonTerminalIndex]
  return "TERMINAL"

suite "Query Engine":
  
  test "Parse simple query":
    let q = newQuery("(program (expression))")
    check q.patterns.len == 1
    check q.patterns[0].value == "program"
    check q.patterns[0].children.len == 1
    check q.patterns[0].children[0].value == "expression"

  test "Parse query with capture":
    let q = newQuery("(binary_op (number) @left)")
    check q.patterns[0].value == "binary_op"
    # (number) @left -> @left applies to (number)
    let child = q.patterns[0].children[0]
    check child.value == "number"
    check child.captureName == "left"

  test "Match simple":
    # Tree: (program (expression (number)))
    let n = makeNode("program", 
              makeNode("expression", 
                makeNode("number")
              )
            )
    
    let q = newQuery("(expression (number))")
    
    var count = 0
    for m in exec(q, n, lookupName):
      count += 1
      check m.captures.len == 0
      
    check count == 1

  test "Match capture":
    let n = makeNode("binary_op", 
              makeNode("number"),
              makeNode("op"),
              makeNode("number")
            )
    
    let q = newQuery("(binary_op (number) @Num)")
    
    var matches = 0
    for m in exec(q, n, lookupName):
      matches += 1
      check m.captures.len >= 1
      check m.captures[0].name == "Num"
      check lookupName(m.captures[0].node.symbol) == "number"
    
    check matches == 1
