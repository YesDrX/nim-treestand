# Edit Functionality Example

This example demonstrates how to use the `edit` procedure to update absolute positions (`startPos`, `endPos`) and `Point` information (row/column) in an existing `ParseNode` tree after the source code has been modified.

Note: `treestand` does not yet support full incremental parsing (re-parsing using the edited tree), but `edit` is the first step in keeping the AST synchronized with source changes.

## Run

```bash
nim r gen.nim
nim r main.nim
```
