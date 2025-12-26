# Getting Started with Treestand

Treestand is a parser generator for Nim, heavily inspired by Tree-sitter. It allows you to define grammars in Nim (or import them from JavaScript), generate efficient GLR parsers, and interact with the resulting syntax trees.

## Installation

### Prerequisites
- **Nim**: Ensure you have Nim installed (version 1.6 or higher recommended).
- **Node.js**: (Optional) Required only if you plan to import existing Tree-sitter grammars defined in `grammar.js`. `Bun` or `Deno` are recommended.

### Installing via Nimble
You can install Treestand directly from the repository or register it locally:

```bash
nimble install treestand
# OR if you have the source locally:
nimble install .
```

## Generate parser.nim from a tree-sitter grammar.js

```nim
import treestand

when isMainModule:
  generateParser(
    grammarPath = "/path/to/tree-sitter-mylang/grammar.js",
    outputDir = "/path/to/output"
  )
```

* External Scanners
  - Some Tree-sitter grammars use a custom C scanner (`scanner.c`) for complex tokens (like Python indentation or Here-docs).

```bash
nim r gen.nim
```

This will produce a `parser.nim` file in the outputDir.

### 2. Use the Parser (`main.nim`)

Now, create a `main.nim` to use your generated parser.

```nim
import treestand
import std/strutils

import parser

proc main() =
  let input = "10 + 2 * 3"
  var myparser = newParser(input)
  
  # Parse the input
  let tree = myparser.parse()
  
  if tree != nil:
    echo "Parsing Successful!"
    # Print the S-Expression (Lisp-like tree structure)
    echo toSExpr(tree)
  else:
    echo "Parsing Failed"

main()
```

* Run the Parser

```bash
nim r main.nim
```

**Output:**
```
Parsing Successful!
(program (expression (binary_op (expression (number "10")) (op "+") (expression (binary_op (expression (number "2")) (op "*") (expression (number "3")))))))
```

Note how the precedence was handled correctly: `2 * 3` is grouped together inside the addition.
