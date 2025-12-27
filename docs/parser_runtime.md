# Parser Runtime API Reference

This document describes the runtime API available in generated parsers. The `parser_runtime.nim` file is **included** (not imported) into generated parsers, making all these functions directly available.

## Overview

The parser runtime provides:
- **Symbol introspection**: Get names and metadata for grammar symbols
- **Tree printing**: Convert parse trees to readable formats (S-expressions, indented text)
- **Tree traversal**: Walk and query the parse tree
- **GLR parsing**: Generic GLR parser with error recovery
- **Convenience API**: High-level node access methods
- **Corpus generation**: Write test cases to corpus files

---

## Symbol Utilities

### `symbolName`

```nim
proc symbolName*(sym: Symbol): string
```

Get the human-readable name of a symbol for debugging.

**Returns:**
- Terminal symbols: Returns the terminal name (e.g., `"number"`, `"+"`, `"tsEOF"`)
- Non-terminal symbols: Returns the non-terminal name (e.g., `"expression"`, `"statement"`)
- Error symbols: Returns `"<error>"`
- Unknown symbols: Returns `"<unknown terminal N>"` or `"<unknown non-terminal N>"`

**Example:**
```nim
let node = parser.parse()
echo symbolName(node.symbol)  # "program"
```

### `isNamed`

```nim
proc isNamed*(sym: Symbol): bool
```

Check if a symbol represents a **named node** in the parse tree.

**Returns:**
- `true` for named terminals (e.g., `identifier`, `number`)
- `true` for all non-terminals
- `false` for anonymous tokens (e.g., `"{"`, `"+"`, keywords)
- `false` for error nodes

**Use case:** Filter out punctuation/keywords when traversing only semantic nodes.

---

## Tree Printing

### `$` (ToString)

```nim
proc `$`*(node: ParseNode): string
```

Convert a parse node to an indented string representation for debugging.

**Format:**
```
program
  statement
    expression
      number "42" [0-2]
```

- Shows hierarchical structure with indentation
- Leaf nodes show: `name text [startPos-endPos]`
- Regex patterns (starting with `/`) are hidden for clarity

**Example:**
```nim
let tree = parser.parse()
echo tree  # Prints indented tree
```

### `toSExpr`

```nim
proc toSExpr*(node: ParseNode, indent: int = 0): string
```

Convert parse node to pretty S-expression format (tree-sitter compatible).

**Format:**
```lisp
(program
  (statement
    (expression
      (number "42"))))
```

**Parameters:**
- `node`: The parse node to convert
- `indent`: Starting indentation level (default: 0)

**Use case:** Generate expected output for corpus tests, compare with tree-sitter parsers.

**Example:**
```nim
let tree = parser.parse()
echo toSExpr(tree)
# Or with custom indent:
echo toSExpr(tree, indent = 2)
```

---

## Tree Traversal

### `walkTree`

```nim
proc walkTree*(node: ParseNode, visit: proc(n: ParseNode) {.closure.})
```

Walk the parse tree in **pre-order** (parent before children), calling `visit` for each node.

**Example:**
```nim
tree.walkTree do (n: ParseNode):
  echo kind(n), ": ", text(n)
```

### `findNodes`

```nim
proc findNodes*(node: ParseNode, predicate: proc(n: ParseNode): bool {.closure.}): seq[ParseNode]
```

Find all nodes matching a predicate function.

**Returns:** Sequence of all matching nodes

**Example:**
```nim
# Find all number literals
let numbers = tree.findNodes do (n: ParseNode) -> bool:
  kind(n) == "number"

for num in numbers:
  echo "Found number: ", text(num)
```

### `findNodesBySymbol`

```nim
proc findNodesBySymbol*(node: ParseNode, symbolNameToFind: string): seq[ParseNode]
```

Find all nodes with a specific symbol name.

**Parameters:**
- `node`: Root node to search from
- `symbolNameToFind`: Exact symbol name to match (e.g., `"identifier"`, `"function_definition"`)

**Example:**
```nim
# Find all function calls
let calls = tree.findNodesBySymbol("function_call")
for call in calls:
  echo "Function call at: ", call.startPos
```

---

## GLR Parser Runtime

### `runGenericGLR`

```nim
proc runGenericGLR*(parser: var Parser, raiseOnFail: bool = false): ParseNode
```

Run the GLR (Generalized LR) parser with advanced error recovery. Usually, we don't use this function directly, instead we use `parse<GrammarName>`, and `proc newParser(source: string): Parser`. `runGenericGLR` can be used for lower level control.

**Parameters:**
- `parser`: The parser instance (with lexer initialized)
- `raiseOnFail`: If `true`, raise exception on parse failure; if `false`, attempt error recovery (default: `false`)

**Returns:** The root `ParseNode` of the parse tree, or `nil` if parsing fails

**Error Recovery Strategies:**

1. **Stack Unwinding (Context Recovery)**
   - When encountering invalid tokens, tries to "break out" of current context
   - Unwinds parse stack to find ancestor state that can handle the token
   - Wraps dropped nodes in an `ERROR` node
   - Rewinds lexer to last "recovery point" (state 0) and tries minimal token skipping

2. **Token Skipping (Fallback)**
   - If unwinding fails, skips the problematic token
   - Creates an `ERROR` node containing the skipped token
   - Continues parsing from the same state

3. **EOF Handling**
   - If EOF is reached and can't be processed, forces accept with best available tree

**Example:**
```nim
var parser = createParser()
parser.lexer.setup(sourceCode)

# With error recovery (default)
let tree = parser.runGenericGLR()
if tree.hasError:
  echo "Warning: Parse tree contains errors"

# Strict mode (raise on error)
try:
  let tree = parser.runGenericGLR(raiseOnFail = true)
  echo "Parsed successfully"
except ValueError:
  echo "Parse failed"
```

---

## Convenience API

### Node Properties

#### `kind`

```nim
proc kind*(node: ParseNode): string
```

Get the node kind (symbol name) as a string.

**Example:**
```nim
assert node.kind == "expression"
```

#### `text`

```nim
proc text*(node: ParseNode): string
```

Get the text content of the node.
- **Terminal nodes**: Returns the token text
- **Non-terminal nodes**: Recursively concatenates text of all children

**Example:**
```nim
let expr = tree.child("expression")
echo expr.text  # "x + 42"
```

#### `capture`

```nim
proc capture*(node: ParseNode): string
```

Syntactic sugar for `text(node)`. Useful for query-like syntax.

**Example:**
```nim
let name = tree.child("identifier").capture
```

### Child Access

#### `child` (by index)

```nim
proc child*(node: ParseNode, index: int): ParseNode
```

Get child at index, returning `nil` if out of bounds.

**Example:**
```nim
let firstChild = node.child(0)
let secondChild = node.child(1)
```

#### `child` (by name)

```nim
proc child*(node: ParseNode, name: string): ParseNode
```

Find **first** child with the given symbol name.

**Returns:** The first matching child, or `nil` if not found

**Example:**
```nim
let funcBody = funcDef.child("block")
let funcName = funcDef.child("identifier")
```

#### `childCount`

```nim
proc childCount*(node: ParseNode): int
```

Get total number of children.

**Example:**
```nim
echo "Node has ", node.childCount, " children"
```

### Named Children

Named children are nodes marked as "named" in the grammar (see [`isNamed`](#isnamed)). This filters out anonymous tokens like punctuation.

#### `namedChildren` (iterator)

```nim
iterator namedChildren*(node: ParseNode): ParseNode
```

Iterate over named children only.

**Example:**
```nim
for child in node.namedChildren:
  echo child.kind
```

#### `namedChild`

```nim
proc namedChild*(node: ParseNode, index: int): ParseNode
```

Get named child at index (skipping unnamed nodes).

**Example:**
```nim
let firstArg = funcCall.namedChild(0)
let secondArg = funcCall.namedChild(1)
```

#### `namedChildCount`

```nim
proc namedChildCount*(node: ParseNode): int
```

Get number of named children.

**Example:**
```nim
echo "Function has ", funcCall.namedChildCount, " arguments"
```

### Error Detection

#### `hasError`

```nim
proc hasError*(node: ParseNode): bool
```

Check if the node or any descendant is an error node.

**Returns:** `true` if any node in the subtree has kind `"ERROR"` or `"MISSING"`

**Example:**
```nim
let tree = parser.parse()
if tree.hasError:
  echo "Parse tree contains errors"
else:
  echo "Clean parse"
```

### Incremental Parsing

#### `edit`

```nim
proc edit*(node: ParseNode, edit: InputEdit)
```

Update node positions to reflect a source code edit. Used for incremental re-parsing.

**Parameters:**
- `node`: Root node to update
- `edit`: Edit descriptor containing:
  - `startByte`: Edit start position
  - `oldEndByte`: Old length
  - `newEndByte`: New length
  - `oldEndPoint`: Old end (row, column)
  - `newEndPoint`: New end (row, column)

**Behavior:**
- Nodes **after** the edit: shifted by delta
- Nodes **overlapping** the edit: end positions updated
- Updates both byte positions and `Point` (row/column) coordinates

**Example:**
```nim
# User inserted 5 characters at position 10
let edit = InputEdit(
  startByte: 10,
  oldEndByte: 0,
  newEndByte: 5,
  oldEndPoint: Point(row: 0, column: 10),
  newEndPoint: Point(row: 0, column: 15)
)
tree.edit(edit)
# Now tree positions reflect the edit
```

---

## Corpus Generation

### `writeCorpus`

```nim
proc writeCorpus*(filename: string, testName: string, input: string, 
                  tree: ParseNode = nil, expectedSExpr: string = "")
```

Write or append a test case to a corpus file (tree-sitter corpus format).

**Parameters:**
- `filename`: Corpus file path
- `testName`: Name of the test case
- `input`: Input source code
- `tree`: Optional parse tree (will be converted to S-expression)
- `expectedSExpr`: Optional expected output (overrides `tree`)

**Corpus Format:**
```
==================
Test Name
==================
input source code
---
(expected
  (s-expression))
```

**Example:**
```nim
# Using parsed tree
let tree = parser.parse()
writeCorpus("tests/corpus.txt", "simple addition", "1 + 2", tree)

# Using explicit expected output
writeCorpus(
  "tests/corpus.txt",
  "error test",
  "invalid syntax",
  expectedSExpr = "(program (ERROR))"
)
```

---

## Iterator API

### `children`

```nim
iterator children*(node: ParseNode): ParseNode
```

Iterate over **all** children (named and unnamed).

**Example:**
```nim
for child in node.children:
  echo child.kind
```

See also: [`namedChildren`](#namedchildren-iterator) for named-only iteration.

---

## Complete Example

```nim
# Generated parser includes parser_runtime automatically
import parser # assume grammar name is Document

let source = """
function add(x, y) {
  return x + y;
}
"""

let tree = parseDocument(source)

# Check for errors
if tree.hasError:
  echo "Parse tree contains errors"
  echo toSExpr(tree)  # Show where errors occurred
else:
  # Traverse named children only
  for funcDef in tree.findNodesBySymbol("function_definition"):
    let funcName = funcDef.child("identifier")
    let params = funcDef.child("parameters")
    
    echo "Function: ", funcName.text
    echo "  Parameters:"
    for param in params.namedChildren:
      echo "    - ", param.text

# Write to corpus for regression testing
writeCorpus("tests/corpus.txt", "function definition", source, tree)
```

---

## See Also

- [Grammar DSL Documentation](using_dsl.md) - Define grammars
- [Examples](../examples/) - Working examples
- [Parser Types](parser_types.nim) - Core data structures (`ParseNode`, `Symbol`, `Token`, etc.)