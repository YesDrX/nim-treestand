# Using `importGrammar` Macro

The `importGrammar` macro is a powerful feature in Treestand that allows you to import Tree-sitter grammars directly into your Nim code at compile-time, generating a complete parser implementation without any intermediate files.

## Overview

Instead of manually generating a `parser.nim` file and importing it, `importGrammar` performs the entire parser generation pipeline at compile-time and injects the generated code directly into your module.

### Compile-Time Pipeline

When you call `importGrammar`, the following happens at compile-time:

1. **Grammar Execution**: Runs the `grammar.js` file using Node.js/Bun/Deno
2. **Grammar Parsing**: Converts the JavaScript grammar definition to Nim structures
3. **Grammar Preparation**: Simplifies and optimizes grammar rules
4. **Table Building**: Generates LR(1) parsing tables
5. **Code Generation**: Creates optimized Nim parser code
6. **Code Injection**: Injects the generated code into your module

All of this happens during compilation, resulting in **zero runtime overhead** for grammar processing.

## Basic Usage

```nim
import treestand

# Import the grammar at compile-time
importGrammar("/path/to/grammar.js")

# Use the generated parser
when isMainModule:
    let input = """{"key": "value", "number": 42}"""
    let tree = parseJson(input)  # parseJson is auto-generated
    echo tree
```

## Using Relative Paths

You can reference grammars relative to your source file using `currentSourcePath`:

```nim
import treestand
import std/os

# Import grammar from relative path
importGrammar(currentSourcePath.parentDir / "grammar.js")

# Or navigate to a specific location
importGrammar(currentSourcePath.parentDir.parentDir / "grammars" / "json" / "grammar.js")
```

## Generated API

After calling `importGrammar`, the following are automatically available in your module:

### Parser Function

```nim
proc parse<GrammarName>(input: string): ParseNode
```

The function name is `parse<GrammarName>` where `<GrammarName>` is the grammar name converted to **CamelCase** (snake_case → CamelCase).

**Naming Examples:**
- Grammar `"json"` → `parseJson(input: string): ParseNode`
- Grammar `"unused_rules"` → `parseUnusedRules(input: string): ParseNodeNode`
- Grammar `"my_lang"` → `parseMyLang(input: string): ParseNodeNode`
- Grammar `"aliased_inlined_rules"` → `parseAliasedInlinedRules(input: string): ParseNode`

### Parser Constructor

```nim
proc newParser(input: string): Parser
```

Unlike the parse function, `newParser` is the **same for all grammars**. It creates a parser instance for more advanced usage.

Usage:
```nim
var parser = newParser(input)
let tree = parser.parse()  # Uses the generic parse method
```

For direct parsing, use the grammar-specific function instead:
```nim
let tree = parseJson(input)  # Easier and more convenient
```


### Node Type

```nim
type
  ParseNode* = ref object
    symbol*: Symbol
    children*: seq[ParseNode]
    token*: Token  # For terminal nodes
    startPos*: int
    endPos*: int
    startPoint*: Point
    endPoint*: Point

  Parser* = object
    lexer*: Lexer
    stacks*: seq[seq[tuple[state: int, node: ParseNode]]]
    lookahead*: Token
```

## Complete Example

Here's a complete example parsing JSON:

```nim
import treestand
import std/os

# Import JSON grammar at compile-time
importGrammar(currentSourcePath.parentDir.parentDir / "tests" / "fixtures" / "json" / "grammar.js")

when isMainModule:
    # Complex JSON input
    let jsonStr = """
    {
        "project": "treestand",
        "version": 1.0,
        "features": [
            "parser-generator",
            "grammar-compilation",
            "runtime-library"
        ],
        "meta": {
            "author": "yesdrx",
            "active": true,
            "score": 99.5,
            "dependencies": null
        },
        "unicode": "\u00A9 2025"
    }
    """
    
    # Parse and display the tree
    let tree = parseJson(jsonStr)
    echo "Parse Tree:"
    echo tree
    echo "Success!"
```

## Advantages

### 1. **Zero Intermediate Files**
No need to create and maintain separate `parser.nim` files. Everything is generated on-the-fly.

### 2. **Compile-Time Validation**
Grammar errors are caught during compilation, not at runtime.

### 3. **No Runtime Overhead**
All grammar processing happens at compile-time. Runtime only does actual parsing.

### 4. **Simplified Workflow**
One-step process: just import the grammar and start parsing.

### 5. **Self-Contained Modules**
Each module that uses `importGrammar` gets its own complete parser implementation.

## Comparison with Manual Generation

### Manual Approach

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

# Step 3: Use parser in main.nim
import parser  # Import the generated file

let tree = parseJson(input)
```

### Using `importGrammar`

```nim
# Single step: import and use
import treestand

importGrammar("grammar.js")

let tree = parseJson(input)
```

## Multiple Grammars

You can import multiple grammars in the same module:

```nim
import treestand
import std/os

# Import JSON grammar
importGrammar(currentSourcePath.parentDir / "json" / "grammar.js")

# Import another grammar (e.g., YAML)
importGrammar(currentSourcePath.parentDir / "yaml" / "grammar.js")

when isMainModule:
    # Use both parsers
    let jsonTree = parseJson("""{"key": "value"}""")
    let yamlTree = parseYaml("key: value")
```

## Path Resolution

The `importGrammar` macro supports several path formats:

### Absolute Paths
```nim
importGrammar("/home/user/grammars/json/grammar.js")
```

### Tilde Expansion
```nim
importGrammar("~/grammars/json/grammar.js")
```

### Relative to Source File
```nim
import std/os
importGrammar(currentSourcePath.parentDir / "grammar.js")
```

### Relative to Project Root
```nim
import std/os
const projectRoot = currentSourcePath.parentDir.parentDir.parentDir
importGrammar(projectRoot / "grammars" / "json" / "grammar.js")
```

## External Scanners

Some Tree-sitter grammars use external scanners (written in C) for complex tokenization. Treestand supports these through external tokens:

```nim
# The grammar.js defines external tokens
# Treestand will generate hooks for them
importGrammar("grammar.js")

# automatically generate the following
{.compile: "path/to/scanner.c".}

# External scanner functions
proc scanner_create*(): pointer {.importc: "tree_sitter_simple_scanner_external_scanner_create".}
proc scanner_destroy*(payload: pointer) {.importc: "tree_sitter_simple_scanner_external_scanner_destroy".}
proc scanner_scan*(payload: pointer, lexer: ptr TSLexer, valid_symbols: ptr bool): bool {.importc: "tree_sitter_simple_scanner_external_scanner_scan".}
proc scanner_serialize*(payload: pointer, buffer: cstring): cuint {.importc: "tree_sitter_simple_scanner_external_scanner_serialize".}
proc scanner_deserialize*(payload: pointer, buffer: cstring, length: cuint) {.importc: "tree_sitter_simple_scanner_external_scanner_deserialize".}
```

The `importGrammar` macro automatically detects external tokens and generates the appropriate hooks.

## Build-Time Messages

When compiling, you'll see informative messages:

```
[Treestand] building from grammar: /path/to/grammar.js
```

This confirms that the grammar is being processed.

## Requirements

- **Node.js, Bun, or Deno**: Required to execute `grammar.js` files
- **Compile-time Path**: The grammar path must be known at compile-time (`static[string]`)

## Best Practices

### 1. Use Constants for Paths

```nim
import std/os

const grammarPath = currentSourcePath.parentDir / "grammar.js"
importGrammar(grammarPath)
```

### 2. One Grammar Per Module (Recommended)

While you can import multiple grammars, it's cleaner to create separate modules:

```nim
# json_parser.nim
import treestand
importGrammar("json/grammar.js")

# yaml_parser.nim
import treestand
importGrammar("yaml/grammar.js")

# main.nim
import json_parser, yaml_parser
```

### 3. Organize Grammar Files

```
project/
├── grammars/
│   ├── json/
│   │   └── grammar.js
│   └── yaml/
│       └── grammar.js
├── src/
│   ├── json_parser.nim
│   ├── yaml_parser.nim
│   └── main.nim
```

### 4. Handle Compilation Time

Parser generation can take a few seconds for complex grammars. For development, consider:

- Using incremental compilation (`nim c --incremental:on`)
- Separating parser modules from main code
- Using Nim's incremental compilation cache

## Debugging

If compilation fails:

1. **Check Grammar Path**: Ensure the path is correct and accessible
2. **Verify Node.js**: Make sure Node.js/Bun/Deno is in your PATH
3. **Grammar Syntax**: Ensure the grammar.js file is valid
4. **Compiler Output**: Check for specific error messages from Treestand

## Performance Considerations

- **Compilation Time**: Complex grammars may increase compile time by a few seconds
- **Runtime Performance**: Zero impact - the generated parser is as fast as manually generated ones
- **Binary Size**: Each imported grammar adds to binary size (similar to manual generation)

## Limitations

1. **Static Paths Only**: Grammar path must be known at compile-time
2. **No Dynamic Loading**: Cannot choose grammars at runtime
3. **Recompilation Required**: Grammar changes require full recompilation

For dynamic grammar loading, use the traditional `generateParser` approach and load the generated parser at runtime.

## See Also

- [Getting Started](getting_started.md) - Traditional parser generation approach
- [Using DSL](using_dsl.md) - Defining grammars in pure Nim
- [Advanced Usage](advanced_usage.md) - Conflict resolution and debugging
