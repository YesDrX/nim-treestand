import std/[strutils]
import parser_types
import sexp_parser

## Query Engine for Tree-stand
## 
## This module implements a Tree-sitter compatible query engine that allows
## matching patterns against a syntax tree using S-expression syntax.
## 
## Usage:
##   1. Create a `Query` object from a source string using `newQuery`.
##   2. Execute the query against a `ParseNode` using the `exec` iterator.
## 
## Example:
##   ```nim
##   let q = newQuery("(program (expression) @my_capture)")
##   for match in exec(q, rootNode, mySymbolToStringFunc):
##     echo match.captures
##   ```

type
  PatternNodeKind* = enum
    ## Distinguishes between different types of pattern nodes.
    pkSymbol      ## Matches a named node (e.g., `(identifier)`).
    pkLiteral     ## Matches an anonymous literal (e.g., `"return"`).
    pkWildcard    ## Matches any node (`_`).

  PatternNode* = ref object
    ## Represents a single node in the query pattern tree.
    kind*: PatternNodeKind
    value*: string          ## The name of the symbol or literal value.
    fieldName*: string      ## The field name required for this node (e.g., `field: (node)`).
    captureName*: string    ## The capture name associated with this node (e.g., `@name`).
    children*: seq[PatternNode] ## Child patterns to match against the node's children.
    isImmediate*: bool      ## If true, this node must be the immediate sibling of the previous node.

  Query* = ref object
    ## A compiled query containing multiple patterns.
    source*: string
    patterns*: seq[PatternNode] ## The list of top-level patterns derived from the source.
    captures*: seq[string]      ## List of all capture names used in the query.

  QueryMatch* = object
    ## Represents a successful match of a pattern against the tree.
    patternIndex*: int          ## The index of the pattern that matched.
    captures*: seq[tuple[name: string, node: ParseNode]] ## Captured nodes for this match.

# --- Parsing Helpers ---

# Forward declarations
proc traverseSexp(node: ParseNode): seq[PatternNode]
proc parseExpression(expr: ParseNode): PatternNode
proc collectExpressions(node: ParseNode, list: var seq[PatternNode])

proc getInnerToken(n: ParseNode): Token =
  ## Recursively extracts the token from a ParseNode.
  if n.children.len > 0: return getInnerToken(n.children[0])
  return n.token

proc collectExpressions(node: ParseNode, list: var seq[PatternNode]) =
  ## Recursively flattens the S-expression tree.
  ## 
  ## The `sexp_parser` generates recursive structures for repeated elements
  ## (e.g., `ntProgram_repeat1`). This function traverses these structures
  ## and collects semantic `Expression` nodes into a flat list.
  
  if node == nil: return
  
  # debugEchoMsg fmt"Visit node: kind={node.symbol.kind}"
  
  if node.symbol.kind == skNonTerminal:
    let idx = node.symbol.nonTerminalIndex
    if idx == sexp_parser.ntExpression.int:
      let p = parseExpression(node)
      if p != nil: list.add(p)
      return
    elif idx == sexp_parser.ntProgram_repeat1.int:
      for c in node.children:
        collectExpressions(c, list)
      return

  # For terminals or other nodes (like List), recurse to find expressions.
  for c in node.children:
    collectExpressions(c, list)

proc parsePatternListContent(items: seq[PatternNode]): seq[PatternNode] =
  ## Processes a flat list of pattern nodes to handle modifiers like fields, captures, and anchors.
  ## 
  ## Modifiers in the S-expression syntax appear as sibling nodes:
  ## - Field: `field_name: (node)` -> `field_name:` is a sibling preceding the node.
  ## - Capture: `(node) @capture` -> `@capture` is a sibling following the node.
  ## - Anchor: `(a) . (b)` -> `.` is a sibling between nodes.
  
  result = @[]
  var i = 0
  while i < items.len:
    let curr = items[i]
    
    # Field: "name:" followed by value
    if curr.kind == pkSymbol and curr.value.endsWith(":"):
      if i + 1 < items.len:
        let val = items[i+1]
        val.fieldName = curr.value[0 .. ^2] # Strip ":"
        result.add(val)
        i += 2
        continue
      else:
        # Dangling field name at end of list
        result.add(curr)
        i += 1
        continue
        
    # Capture: "@name" follows value
    if curr.kind == pkSymbol and curr.value.startsWith("@"):
       if result.len > 0:
         result[^1].captureName = curr.value[1 .. ^1] # Strip "@"
       i += 1
       continue
       
    # Anchor: "."
    if curr.kind == pkWildcard and curr.value == ".":
       # Anchor operator (.) enforces immediate sibling relationship.
       if i + 1 < items.len:
          items[i+1].isImmediate = true
       i += 1
       continue
       
    # Normal node
    result.add(curr)
    i += 1

proc parseExpression(expr: ParseNode): PatternNode =
  ## Converts a parsed S-expression node into a `PatternNode`.
  if expr.children.len == 0: return nil
  
  let content = expr.children[0]
  
  if content.symbol.kind == skTerminal:
    let idx = content.symbol.terminalIndex
    
    if idx == sexp_parser.tsAtom.int:
       return PatternNode(kind: pkSymbol, value: content.token.text, children: @[])
    elif idx == sexp_parser.tsWildcard.int:
       return PatternNode(kind: pkWildcard, value: "_", children: @[])
    elif idx == sexp_parser.tsCapture.int:
       return PatternNode(kind: pkSymbol, value: content.token.text, children: @[])
    elif idx == sexp_parser.tsField.int:
       return PatternNode(kind: pkSymbol, value: content.token.text, children: @[])
    elif idx == sexp_parser.tsAnchor.int:
       return PatternNode(kind: pkWildcard, value: ".", children: @[])
    
    return nil

  let idx = content.symbol.nonTerminalIndex
  
  if idx == sexp_parser.ntList.int:
    # List: ( ... )
    var items: seq[PatternNode] = @[]
    collectExpressions(content, items)
    let processedChildren = parsePatternListContent(items)
    
    if processedChildren.len > 0 and processedChildren[0].kind == pkSymbol:
       let head = processedChildren[0]
       head.children = processedChildren[1 .. ^1]
       return head
    else:
      return nil

  elif idx == sexp_parser.ntAtom.int:
    let val = getInnerToken(content).text
    return PatternNode(kind: pkSymbol, value: val, children: @[])
    
  elif idx == sexp_parser.ntString.int:
    # String literal in query: "foo"
    # Content of string rule
    let val = getInnerToken(content).text
    # strip quotes "..."
    if val.len >= 2:
      return PatternNode(kind: pkLiteral, value: val[1 .. ^2], children: @[])
    return PatternNode(kind: pkLiteral, value: val, children: @[])
    
  elif idx == sexp_parser.ntWildcard.int:
    return PatternNode(kind: pkWildcard, value: "_", children: @[])
    
  elif idx == sexp_parser.ntCapture.int:
    let val = getInnerToken(content).text
    return PatternNode(kind: pkSymbol, value: val, children: @[])
    
  elif idx == sexp_parser.ntField.int:
    let val = getInnerToken(content).text
    return PatternNode(kind: pkSymbol, value: val, children: @[])
    
  elif idx == sexp_parser.ntAnchor.int:
    return PatternNode(kind: pkWildcard, value: ".", children: @[])
    
  return nil

proc traverseSexp(node: ParseNode): seq[PatternNode] =
  ## Parses the entire S-expression program into a sequence of top-level patterns.
  var items: seq[PatternNode] = @[]
  collectExpressions(node, items)
  return parsePatternListContent(items)

# --- Main API ---

proc newQuery*(source: string): Query =
  ## Compiles a query source string into a `Query` object.
  ## 
  ## The source is parsed using the internal S-expression parser.
  ## If parsing fails, the resulting Query will have no patterns (and potentially print errors currently).
  var parser = sexp_parser.newParser(source)
  var tree = parser.parse()
  result = Query(source: source, patterns: @[], captures: @[])
  
  if tree != nil:
    result.patterns = traverseSexp(tree)

# --- Execution ---

proc matchPattern(node: ParseNode, pat: PatternNode, symName: proc(s: Symbol): string): bool =
  ## Checks if a single `PatternNode` matches a given `ParseNode`.
  let nodeName = symName(node.symbol)
  
  case pat.kind
  of pkWildcard: discard # Matches anything
  of pkSymbol:
    if nodeName != pat.value: return false
  of pkLiteral:
    # Literal matches exact token text (e.g. keywords, punctuation)
    if node.token.text != pat.value: return false

  if pat.children.len > 0:
    # If pattern has children, we must match them against the node's children.
    if node.children.len == 0: return false
    
    var childIdx = 0
    for patChild in pat.children:
      var found = false
      while childIdx < node.children.len:
        # TODO: This logic simply searches for *any* matching child (search).
        # It does not strictly enforce direct sibling relationships unless `isImmediate` is set.
        # Future optimization: Implement optimized child cursor.
        if matchPattern(node.children[childIdx], patChild, symName):
          found = true
          childIdx += 1
          break
        childIdx += 1
      if not found: return false
  return true

proc collectCaptures(n: ParseNode, p: PatternNode, symName: proc(s: Symbol): string, 
                     captures: var seq[tuple[name: string, node: ParseNode]]) =
  ## Recursively collects captures from a matched node/pattern pair.
  if p.captureName.len > 0:
    captures.add((p.captureName, n))
  
  if p.children.len > 0:
    var cIdx = 0
    for pc in p.children:
      while cIdx < n.children.len:
        if matchPattern(n.children[cIdx], pc, symName):
          collectCaptures(n.children[cIdx], pc, symName, captures)
          cIdx += 1
          break
        cIdx += 1

proc validMatch(node: ParseNode, pat: PatternNode, symName: proc (s: Symbol): string, 
                captures: var seq[tuple[name: string, node: ParseNode]]): bool =
  ## Helper to validate a match and collect its captures.
  if matchPattern(node, pat, symName):
    collectCaptures(node, pat, symName, captures)
    return true
  return false

iterator exec*(query: Query, node: ParseNode, symName: proc(s: Symbol): string): QueryMatch =
  ## Iterates over all matches of the query within the given syntax tree `node`.
  ## 
  ## `symName` is a callback that resolves a `Symbol` to its string name (e.g., "program", "identifier").
  ## This is required because `ParseNode` stores integer symbols, while the query uses string names.
  var stack = @[node]
  while stack.len > 0:
    let curr = stack.pop()
    
    for i, pat in query.patterns:
      var caps: seq[tuple[name: string, node: ParseNode]] = @[]
      if validMatch(curr, pat, symName, caps):
        yield QueryMatch(patternIndex: i, captures: caps)
    
    # DFS Traversal
    for i in countdown(curr.children.len - 1, 0):
      stack.add(curr.children[i])
