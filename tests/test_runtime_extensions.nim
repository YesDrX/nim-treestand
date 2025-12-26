import treestand/parser_types
import std/[strutils, options, os]
import unittest
import treestand/corpus

# Mock constants required by parser_runtime
const terminalSymbolNames* = ["EOF", "TestTerm"]
const nonTerminalSymbolNames* = ["TestNonTerm"]

const terminalSymbolMetadata* = [
  SymbolMetadata(named: false), # EOF
  SymbolMetadata(named: true)   # TestTerm
]

const nonTerminalSymbolMetadata* = [
  SymbolMetadata(named: true)   # TestNonTerm
]

const externalTokenBase* = 2
const externalExtraTokens*: set[int16] = {}

const parseTableIndex* = [
  (actionStart: 0, actionLen: 0, gotoStart: 0, gotoLen: 0, lexState: 0)
]
const parseTableActions* = newSeq[(Symbol, ParseAction)]()
const parseTableGotos* = newSeq[(Symbol, uint32)]()

proc nextToken*(lexer: Lexer, validExternalSymbols: set[int16] = {}, lexState: int = 0): Token =
  Token(kind: terminal(0), text: "", startPos: 0, endPos: 0)


# Include the runtime to test extensions
include treestand/parser_runtime

suite "Runtime Extensions":
  test "Node accessors":
    # Construct a tree manually
    # TestNonTerm -> TestTerm("hello")
    
    let tokenTerm = Token(kind: terminal(1), text: "hello", startPos: 0, endPos: 5)
    let nodeTerm = ParseNode(symbol: terminal(1), children: @[], token: tokenTerm, startPos: 0, endPos: 5)
    
    let nodeRoot = ParseNode(symbol: nonTerminal(0), children: @[nodeTerm], startPos: 0, endPos: 5)
    
    # Test kind
    check kind(nodeRoot) == "TestNonTerm"
    check kind(nodeTerm) == "TestTerm"
    
    # Test text
    check text(nodeTerm) == "hello"
    check text(nodeRoot) == "hello"
    
    # Test child access
    check childCount(nodeRoot) == 1
    check child(nodeRoot, 0) == nodeTerm
    check child(nodeRoot, 1) == nil
    
    # Test iterators
    var childrenSeq: seq[ParseNode] = @[]
    for c in children(nodeRoot):
      childrenSeq.add(c)
    check childrenSeq.len == 1
    check childrenSeq[0] == nodeTerm
    
    var namedChildrenSeq: seq[ParseNode] = @[]
    for c in namedChildren(nodeRoot):
      namedChildrenSeq.add(c)
    check namedChildrenSeq.len == 1
    check namedChildrenSeq[0] == nodeTerm
    
    # Test named child access
    check namedChildCount(nodeRoot) == 1
    check namedChild(nodeRoot, 0) == nodeTerm

  test "Write Corpus from Runtime":
    let fname = "tests/temp_corpus.txt"
    if fileExists(fname): removeFile(fname)
    
    # Test writing with string expectation
    writeCorpus(fname, "My Test Case", "func main() {}", expectedSExpr = "(program)")
    
    let content = readFile(fname)
    check content.contains("My Test Case")
    check content.contains("func main() {}")
    check content.contains("(program)")
    
    # Test writing with ParseNode
    # Construct a tree manually: TestNonTerm -> TestTerm("hello")
    let tokenTerm = Token(kind: terminal(1), text: "hello", startPos: 0, endPos: 5)
    let nodeTerm = ParseNode(symbol: terminal(1), children: @[], token: tokenTerm, startPos: 0, endPos: 5)
    let nodeRoot = ParseNode(symbol: nonTerminal(0), children: @[nodeTerm], startPos: 0, endPos: 5)
    
    writeCorpus(fname, "Node Test", "hello", tree = nodeRoot)
    
    let content2 = readFile(fname)
    check content2.contains("Node Test")
    # Check for S-expression format parts since toSExpr is pretty-printed (multiline)
    check content2.contains("(TestNonTerm")
    check content2.contains("(TestTerm \"hello\")")

    removeFile(fname)

  test "Edit functionality":
    # Construct: (Root (Term1 "hello") (Term2 "there"))
    # Term1: 0..5, Row 0, Col 0..5
    # space: 5..6 (implicitly skipped or handled)
    # Term2: 6..11, Row 0, Col 6..11
    
    let p0 = Point(row: 0, column: 0)
    let p5 = Point(row: 0, column: 5)
    let p6 = Point(row: 0, column: 6)
    let p11 = Point(row: 0, column: 11)
    
    let t1 = Token(kind: terminal(1), text: "hello", startPos: 0, endPos: 5, startPoint: p0, endPoint: p5)
    let n1 = ParseNode(symbol: terminal(1), children: @[], token: t1, startPos: 0, endPos: 5, startPoint: p0, endPoint: p5)
    
    let t2 = Token(kind: terminal(1), text: "there", startPos: 6, endPos: 11, startPoint: p6, endPoint: p11)
    let n2 = ParseNode(symbol: terminal(1), children: @[], token: t2, startPos: 6, endPos: 11, startPoint: p6, endPoint: p11)
    
    # Root covers 0..11
    let root = ParseNode(symbol: nonTerminal(0), children: @[n1, n2], startPos: 0, endPos: 11, startPoint: p0, endPoint: p11)
    
    # Edit: Insert " big " at 5 (after hello). Length 5.
    # Original: "hello there"
    # New: "hello big there"
    # Insertion at 5. 
    # startByte: 5. 
    # oldEndByte: 0 (inserting). 
    # newEndByte: 5 (" big ").
    # oldEndPoint: (0, 5).
    # newEndPoint: (0, 10). (5 chars added to col 5 -> 10)
    
    var editOp = InputEdit(
      startByte: 5,
      oldEndByte: 0, 
      newEndByte: 5,
      startPoint: Point(row: 0, column: 5),
      oldEndPoint: Point(row: 0, column: 5),
      newEndPoint: Point(row: 0, column: 10)
    )
    
    edit(root, editOp)
    
    # Validation
    # n1 (0..5) should be unchanged strictly (it's slightly before/at start). 
    # Our logic: endPos > startByte? 5 > 5 False. So n1 unchanged.
    check n1.startPos == 0
    check n1.endPos == 5
    
    # n2 (was 6..11) should shift by 5.
    # New start: 11. New end: 16.
    check n2.startPos == 11
    check n2.endPos == 16
    
    # Points
    # n2 startPoint was (0,6). Should be (0, 11).
    check n2.startPoint.row == 0
    check n2.startPoint.column == 11
    
    # Root endPos was 11. Should be 16.
    # Root overlaps edit (0..11 contains 5).
    check root.endPos == 16
