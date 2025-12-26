# Example 09: AST Construction with Embedded Actions

This example demonstrates how to use **embedded actions** in `tsGrammar` to build a custom Abstract Syntax Tree (AST) during parsing.

## What This Example Shows

1. **Embedded Actions**: Nim code blocks attached to grammar rules
2. **Bottom-Up AST Construction**: Using a `Table[ParseNode, Expr]` to build recursive structures
3. **Node API**: Accessing parse tree nodes via `node.children`, `node.text`, etc.
4. **Pattern**: Table-based approach for recursive data structures

## The Pattern

### 1. Define Your AST

```nim
type
  Expr = ref object
    case kind: ExprKind
    of ekInt: intVal: int
    of ekBinary:
      op: string
      left, right: Expr
```

### 2. Create Builder with Table

```nim
type
  AstBuilder = object
    astMap: Table[ParseNode, Expr]
    rootAst: Expr  # Final result
```

### 3. Attach Actions to Rules

```nim
tsGrammar "math", userdata: AstBuilder:
  # Leaf nodes: create AST directly
  number <- token(re"\d+"):
    userdata.astMap[node] = Expr(
      kind: ekInt,
      intVal: parseInt(node.text)
    )
  
  # Branch nodes: lookup children from table
  binary_op <- expression * op * expression:
    let left = userdata.astMap[node.children[0]]
    let right = userdata.astMap[node.children[2]]
    userdata.astMap[node] = Expr(
      kind: ekBinary,
      op: node.children[1].text,
      left: left,
      right: right
    )
  
  # Root: store final result
  program <- expression:
    userdata.rootAst = userdata.astMap[node.children[0]]
```

### 4. Parse and Get AST

```nim
var builder = AstBuilder()
if matchMath("1 + 2 * 3", builder):
  echo builder.rootAst  # ((1 + 2) * 3)
```

## How It Works

1. **Post-Order Traversal**: Actions execute bottom-up (children before parents)
2. **Leaf Nodes First**: `number` actions create `Expr` objects
3. **Branch Nodes**: `binary_op` looks up children in table, combines them
4. **Propagation**: Intermediate rules like `expression` copy child AST upward
5. **Root Capture**: Top-level `program` stores final AST in `rootAst`

## Running the Example

```bash
cd examples/09_ast_actions
nim r main.nim
```

Expected output:
```
=== AST Construction with Embedded Actions ===

Example 1: 1 + 2
  Root AST: (1 + 2)
  Expected: (1 + 2)

Example 2: 1 + 2 * 3
  Root AST: ((1 + 2) * 3)
  Expected: ((1 + 2) * 3)  # Left-associative

Example 3: (1 + 2) * 3
  Root AST: ((1 + 2) * 3)
  Expected: ((1 + 2) * 3)

Example 4: 1 + 2 * 3 - 4 / 5
  Root AST: ((((1 + 2) * 3) - 4) / 5)

=== Complete ===
```

## Key Takeaways

- **Use actions for**: AST building, semantic validation, symbol tables
- **Table pattern**: Best for recursive structures (AST, IR)
- **Node API**: Simple, predictable access to parse tree
- **Execution order**: Bottom-up ensures children are ready before parents

## Comparison with Traditional Approach

The traditional approach (see [`example 08`](../08_ast_traversal)) uses a separate `toAst(node): Expr` function. Both are valid:

| Approach | Best For |
|----------|----------|
| **Embedded Actions** | Imperative transformations, side effects, symbol tables |
| **Separate Function** | Functional transformations, clean separation of concerns |

Choose based on your use case!

## See Also

- [Using the DSL](../../docs/using_dsl.md#embedded-actions) - Full documentation
- [Example 07](../07_dsl_macro) - Basic `tsGrammar` usage
- [Example 08](../08_ast_traversal) - Traditional AST construction
