# Basic Grammar Example

This example demonstrates the core workflow:
1.  Defining a grammar using a Nim DSL (helper procs).
2.  Generating a Nim parser from the grammar.
3.  Including the generated parser.
4.  Parsing a string.
5.  Traversing the resulting `ParseNode` tree.

## Run

```bash
nim r gen.nim
nim r main.nim
```
