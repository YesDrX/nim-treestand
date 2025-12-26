# Example 05: Using `importGrammar` Macro

This example demonstrates how to use Treestand's `importGrammar` macro to compile and import a Tree-sitter grammar at compile-time, without generating intermediate parser files.

## What This Example Does

This example shows how to:
1. Import a JSON grammar using the `importGrammar` macro
2. Parse JSON input using the auto-generated parser
3. Display the parse tree

## Key Features

### Compile-Time Grammar Import

Instead of manually generating a `parser.nim` file, the `importGrammar` macro:
- Executes the `grammar.js` file at compile-time
- Builds the parsing tables
- Generates optimized parser code
- Injects it directly into your module

All of this happens during compilation, with **zero runtime overhead**.

### Self-Contained Code

The entire example is self-contained in a single file - no separate parser generation step required.

## Running the Example

```bash
cd examples/05_importGrammar
nim r main.nim
```

## Expected Output

```
[Treestand] building from grammar: ../../../tests/fixtures/json/grammar.js
Parse Tree:
(document (object ...))
========================================
Success!
```

## Code Walkthrough

### 1. Import the Grammar

```nim
import treestand
import std/os

importGrammar(currentSourcePath.parentDir.parentDir.parentDir / "tests" / "fixtures" / "json" / "grammar.js")
```

**What happens here:**
- `currentSourcePath.parentDir.parentDir.parentDir` navigates to the project root
- The path points to the JSON grammar in the test fixtures
- At compile-time, Treestand processes the grammar and generates the parser

### 2. Use the Generated Parser

```nim
let tree = parseJson(jsonStr)
```

**Auto-generated API:**
- `parseJson(input: string): Node` - Parse JSON and return the syntax tree
- The function name is `parse<GrammarName>` where the grammar name is converted to CamelCase
- Examples: `json` → `parseJson`, `my_lang` → `parseMyLang`, `unused_rules` → `parseUnusedRules`


### 3. Display the Tree

```nim
echo tree
```

The parse tree is displayed in S-expression format, showing the hierarchical structure of the parsed JSON.

## Advantages Over Manual Generation

### Traditional Approach
```nim
# Step 1: Create gen.nim
import treestand
when isMainModule:
    generateParser(
        grammarPath = "grammar.js",
        outputDir = "."
    )

# Step 2: Run generator
# $ nim r gen.nim

# Step 3: Use in main.nim
import parser
let tree = parseJson(input)
```

### Using `importGrammar`
```nim
# Single step
import treestand
importGrammar("grammar.js")
let tree = parseJson(input)
```

## Key Concepts

### Compile-Time vs Runtime

| Phase | Traditional Approach | `importGrammar` Macro |
|-------|---------------------|----------------------|
| Grammar Processing | Manual step (run gen.nim) | Automatic (compile-time) |
| Parser Generation | Creates parser.nim file | Embedded in module |
| File Management | 2+ files (gen.nim, parser.nim, main.nim) | 1 file (main.nim) |
| Runtime Overhead | None (after generation) | None |

### Path Resolution

The example uses `currentSourcePath.parentDir` to construct relative paths:

```nim
currentSourcePath              # /path/to/examples/05_importGrammar/main.nim
.parentDir                     # /path/to/examples/05_importGrammar
.parentDir                     # /path/to/examples
.parentDir                     # /path/to (project root)
/ "tests/fixtures/json/grammar.js"  # Full path to grammar
```

This ensures the example works regardless of where it's placed in the filesystem.

## Customizing for Your Grammar

To use this pattern with your own grammar:

```nim
import treestand
import std/os

# Option 1: Absolute path
importGrammar("/absolute/path/to/grammar.js")

# Option 2: Relative to source file
importGrammar(currentSourcePath.parentDir / "grammar.js")

# Option 3: Tilde expansion
importGrammar("~/grammars/mygrammar/grammar.js")

when isMainModule:
    # Use the generated parser (name depends on grammar)
    let tree = parseMyGrammar(input)
    echo tree
```

## Generated Parser API

The `importGrammar` macro generates several functions and types:

### Main Parser Function
```nim
proc parseJson(input: string): Node
```
- Takes input string
- Returns the root node of the parse tree
- Name is `parse<GrammarName>` where the grammar name is converted to **CamelCase**

**Naming Examples:**
- `"json"` → `parseJson()`
- `"yaml"` → `parseYaml()`
- `"unused_rules"` → `parseUnusedRules()`
- `"my_language"` → `parseMyLanguage()`
- `"aliased_inlined_rules"` → `parseAliasedInlinedRules()`

### Parser Constructor
```nim
proc newParser(input: string): Parser
```
- Creates a parser instance for more advanced usage
- **Note:** `newParser` is the same for all grammars (not grammar-specific)
- Useful for incremental parsing or custom error handling

Example:
```nim
var parser = newParser(input)
let tree = parser.parse()  # Generic parse method
```

### Node Type
```nim
type Node = ref object
    kind: string        # Node type (e.g., "object", "array", "string")
    text: string        # Original text
    children: seq[Node] # Child nodes
```

## Requirements

- **Node.js, Bun, or Deno**: Required to execute the `grammar.js` file
- **Treestand**: Must be installed via `nimble install treestand`

## Compilation Time

Grammar processing happens at compile-time, so the first compilation may take a few seconds. Subsequent compilations use Nim's incremental compilation cache and are much faster if the grammar hasn't changed.

## See Also

- [docs/import_grammar.md](../../docs/import_grammar.md) - Comprehensive guide to `importGrammar`
- [docs/getting_started.md](../../docs/getting_started.md) - Traditional parser generation approach
- [Example 01](../01_basic) - Basic parser usage with manual generation
