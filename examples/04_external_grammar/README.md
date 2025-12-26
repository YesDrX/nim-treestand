# External Grammar & Scanner Example

This example demonstrates how to use `treestand` with standard Tree-sitter input files:
1.  `grammar.js`: Defines the grammar using JavaScript logic.
2.  `src/scanner.c`: Defines an external scanner for custom tokenization (comments).

The `gen.nim` script uses `treestand`'s Javascript execution capability (requiring `node` or `bun`) to evaluate `grammar.js` and generate the parser.

## Run

```bash
nim r gen.nim
nim r main.nim
```
