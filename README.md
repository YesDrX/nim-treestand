# Treestand

Treestand is a complete re-implementation of the [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) parser generator in Nim. It takes `grammar.js`, or grammar rules defined in Nim and optionally `scanner.c` as input and generates high-performance Nim parsers automatically.

**Treestand leverages Nim's powerful metaprogramming capabilities** to make parser generation seamless and efficient:
- **Compile-time macros** (`importGrammar`, `buildGrammar`) that generate parsers directly in your code
- **Zero runtime overhead** - all grammar processing happens at compile-time
- **Type-safe DSL** for defining grammars in pure Nim with full IDE support

## Purpose

The goal of Treestand is to provide a native Nim implementation of the Tree-sitter ecosystem, matching its rigorous standards for conflict detection and parsing performance, while eliminating the need for a C runtime during the generation phase (though `scanner.c` files are still supported).

## Features

- **Direct Execution**: Executes `grammar.js` using Bun or Node.js via an integrated DSL.
- **Full Compatibility**: Closely follows Tree-sitter's internal algorithms (NFA, DFA, LR(1) table construction).
- **Nim-Native**: Generates pure Nim code for the parser and runtime.
- **External Scanners**: Native support for `scanner.c` files using Nim's `{.compile.}` and `importc` pragmas.
- **Excellent Conflict Detection**: Accurate reporting of Shift/Reduce and Reduce/Reduce conflicts.

## Installation

```bash
nimble install treestand
```

Or install from source

```bash
nimble install https://github.com/YesDrX/nim-treestand
```

## CLI Usage

Treestand provides a unified CLI for generating and testing parsers.

### Generate a Parser

```bash
treestand --cmd generate --grammarPath grammar.js --outputDir ./generated
```

**Options:**
- `--grammarPath`: Path to the `grammar.js` file.
- `--outputDir`: Output directory for the generated `parser.nim` (default: `.`).
- `--dslPath`: (Optional) Explicit path to `dsl.js`.
- `--parserName`: (Optional) Custom name for the generated parser.

### Test a Grammar

```bash
treestand --cmd test --fixtureDir ./tests/fixtures/my_grammar
```

This will generate the parser, compile it with a test runner, and verify it against `corpus.txt`.

## Library APIs

Treestand is also available as a library. The main entry point is `treestand.nim`.

```nim
import treestand

when isMainModule:
  generateParser(
    grammarPath = "/path/to/tree-sitter-mylang/grammar.js",
    outputDir = "/path/to/output"
  )
```

### Using `ts_grammar` Macro (Recommended)

The easiest way to define grammars is using the `ts_grammar` macro with a concise, PEG/EBNF-like syntax:

```nim
import treestand/dsl_macros
import treestand/dsl
import std/os

# Define grammar using clean DSL syntax
ts_grammar "math":
  # Entry point - one or more statements
  program     <- +statement
  
  # Choice between different statement types
  statement   <- assignment | expression
  
  # Named fields for AST nodes
  assignment  <- (target: identifier) * eq * (value: expression)
  
  # Precedence and associativity
  expression  <- number | binary_op | parens
  binary_op   <- ((left: expression) * op * (right: expression)) ^ 1
  parens      <- lparen * expression * rparen
  
  # Lexical tokens - use token() wrapper
  identifier  <- token(re"[a-zA-Z_]\w*")
  number      <- token(re"\d+")
  op          <- token(re"[+\-*/]")
  eq          <- token("=")
  lparen      <- token("(")
  rparen      <- token(")")
  
  # Keywords using set syntax
  keyword     <- {"if", "else", "while"}
  
  # Configuration - skip whitespace
  extras      = token(re"\s+")

# The macro generates a function to create the grammar
when isMainModule:
  let g = math()
  generateParser(g, currentSourcePath().parentDir() / "parser.nim")
  echo "Parser generated!"
```

**Benefits:**
- **Clean Syntax**: Readable PEG/EBNF-like notation
- **Named Fields**: Easy AST node field access with `(name: rule)`
- **Operator Sugar**: `*` for sequence, `|` for choice, `+`/`*`/`?` for repetition
- **Set Syntax**: `{"a", "b"}` for keyword choices
- **No JavaScript**: Pure Nim, zero dependencies
- **Compile-time**: All generation happens at compile-time

See [docs/using_dsl.md](docs/using_dsl.md) for the complete macro reference.

### Using `importGrammar` Macro

For existing Tree-sitter grammars, use `importGrammar` to generate parsers from `grammar.js` files:

```nim
import treestand
import std/os

# Import grammar at compile-time - generates parser directly in your module
importGrammar(currentSourcePath.parentDir / "grammar.js")

when isMainModule:
  # Use the auto-generated parser
  let tree = parseJson("""{"key": "value", "number": 42}""")
  echo tree
```

**Benefits:**
- No intermediate `parser.nim` files
- All parser generation happens at compile-time
- Zero runtime overhead
- Self-contained modules

See [docs/import_grammar.md](docs/import_grammar.md) for detailed documentation.

### Using `buildGrammar` Macro

For the ultimate in simplicity, use `buildGrammar` to define grammars in pure Nim without any JavaScript dependencies:

```nim
import treestand
import std/options

# Define grammar using pure Nim DSL
proc createMathGrammar(): InputGrammar =
  InputGrammar(
    name: "math",
    variables: @[
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),
      Variable(name: "expression", kind: vtNamed, 
               rule: choice(sym("number"), sym("binary_op"))),
      Variable(name: "binary_op", kind: vtNamed,
               rule: prec_left(1, seq(sym("expression"), sym("op"), sym("expression")))),
      Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+"))),
      Variable(name: "op", kind: vtNamed, rule: token(patt("[+\\-*/]")))
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )

# Build parser from pure Nim grammar
buildGrammar(createMathGrammar)

when isMainModule:
  let tree = parseMath("1 + 2 * 3")
  echo tree
```

**Benefits:**
- **No JavaScript**: Zero JavaScript dependencies
- **Type-safe DSL**: Full Nim type checking and IDE support
- **Single file**: Grammar and code in one place
- **Compile-time**: All generation at compile-time

See [docs/build_grammar.md](docs/build_grammar.md) for detailed documentation.



## Project Structure

- `src/treestand.nim` - Main entry point (CLI & Library Exports)
- `src/treestand/cli/` - CLI implementation
  - `generate.nim` - Parser generation command
  - `test.nim` - Grammar testing command
- `src/treestand/` - Core library modules
  - `pragmas.nim` - **Compile-time macros** (`importGrammar`, `buildGrammar`)
  - `grammar.nim` - Grammar types and structures (`InputGrammar`, `Variable`, `Rule`)
  - `dsl.nim` - Pure Nim DSL for grammar definitions
  - `parse_grammar.nim` - JSON grammar parser (for grammar.js)
  - `prepare_grammar.nim` - Grammar preparation (flattening, inlining, optimization)
  - `build_tables.nim` - NFA/DFA/LR(1) table construction
  - `codegen.nim` - Nim code generation for parsers
  - `js_exec.nim` - JavaScript execution engine (for grammar.js)
  - `query.nim` - Tree-sitter compatible query engine
  - `parser_types.nim` - Parser runtime types
  - `parser_runtime.nim` - Parser runtime implementation
  - `dsl.js` - JavaScript DSL used during grammar.js execution
- `docs/` - Documentation
  - `getting_started.md` - Installation and first parser
  - `import_grammar.md` - `importGrammar` macro guide
  - `build_grammar.md` - `buildGrammar` macro guide
  - `using_dsl.md` - Nim DSL reference
  - `query.md` - Query engine documentation
  - `advanced_usage.md` - Conflict resolution and debugging
- `examples/` - Example projects
  - `05_importGrammar/` - Using `importGrammar` with grammar.js
  - `06_buildGrammar/` - Using `buildGrammar` with pure Nim


## License

MIT

