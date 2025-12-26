# AST Traversal Example

This example demonstrates how to:
1.  Define a grammar using the `tsGrammar` macro (pure Nim DSL).
2.  Parse an input string into a concrete syntax tree (CST) using `treestand`.
3.  Traverse the CST (`ParseNode` tree) and map it to a custom Nim AST.

## Files

- `main.nim`: The complete example containing the grammar, AST definition, traversal logic, and main execution.

## Key Concepts

- **`tsGrammar`**: Generating a parser directly in Nim code.
- **`ParseNode`**: The raw tree structure returned by `treestand`.
- **`NonTerminalSymbol` / `TerminalSymbol`**: Generated enums used to safely identify node types during traversal.
- **Recursion**: Building the AST by recursively visiting children nodes.

## Usage

Run the example using Nim:

```bash
nim r examples/08_ast_traversal/main.nim
```

## Output

The example parses the expression `1 + 2 * 3` and prints the resulting custom AST, demonstrating operator precedence structure.
