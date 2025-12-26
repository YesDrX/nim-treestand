# Treestand Architecture

Treestand is a complete re-implementation of the Tree-sitter parser generator in Nim. This document outlines its internal structure and how it relates to the original Tree-sitter implementation in Rust.

## Overview

Treestand follows a pipeline architecture:
1. **JavaScript Execution**: `grammar.js` → JSON
2. **Grammar Parsing**: JSON → `InputGrammar`
3. **Grammar Preparation**: `InputGrammar` → `SyntaxGrammar` + `LexicalGrammar`
4. **Table Building**: Grammars → `ParseTable` + `LexTable`
5. **Code Generation**: Tables → `parser.nim`

## Components

### 1. JavaScript Execution (`src/treestand/js_exec.nim`)
- Locates an external JavaScript engine (Bun or Node.js).
- Executes `grammar.js` using the internal `src/treestand/dsl.js`.
- Produces a standardized JSON representation of the grammar.

### 2. Grammar Parsing (`src/treestand/parse_grammar.nim`)
- Parses the JSON output into strongly-typed Nim structures (`InputGrammar`).
- Validates the rule tree and extracts metadata.

### 3. Grammar Preparation (`src/treestand/prepare_grammar.nim`)
- Matches the logic of `tree-sitter/lib/src/prepare_grammar/`.
- Flattens rules, inlines variables, and expands repeats.
- Separates the grammar into a **Syntax Grammar** (for LR parsing) and a **Lexical Grammar** (for the DFA lexer).

### 4. Table Building (`src/treestand/build_tables.nim`)
- **Lexical Analysis**: Builds an NFA representing all tokens, then converts it to a DFA using subset construction.
- **Syntactic Analysis**: Computes FIRST and FOLLOW sets, then builds an LR(1) parse table.
- **Conflict Resolution**: Implements Tree-sitter's precedence and associativity rules to resolve Shift/Reduce and Reduce/Reduce conflicts.

### 5. Code Generation (`src/treestand/codegen.nim`)
- Generates a standalone Nim file containing the parse tables and the parser runtime.
- Handles `scanner.c` integration via `{.compile.}` and `importc` declarations.

## Rust-to-Nim Mapping

Treestand is designed to be a faithful port of Tree-sitter. Below is a mapping of key components from the Rust codebase to Treestand:

| Tree-sitter (Rust) | Treestand (Nim) | Notes |
|-------------------|-----------------|-------|
| `lib/src/parse_grammar/` | `src/treestand/parse_grammar.nim` | |
| `lib/src/prepare_grammar/` | `src/treestand/prepare_grammar.nim` | flattening, inlining, etc. |
| `lib/src/build_tables/` | `src/treestand/build_tables.nim` | NFA, DFA, LR(1) construction |
| `lib/src/generate/` | `src/treestand/codegen.nim` | Equivalent to `render.rs` |
| `cli/src/generate.rs` | `src/treestand/cli/generate.nim` | |
| `cli/src/test.rs` | `src/treestand/cli/test.nim` | |
| `lib/src/parser.c` | `src/treestand/parser_runtime.nim` | Included in generated output |

## Implementation Status

Treestand is **fully implemented** and passes 100% of its grammar regression suite.

- ✅ **Project structure**: Reorganized CLI and library split.
- ✅ **JS Execution**: Fully functional with Bun/Node.
- ✅ **Table Building**: LR(1) and DFA construction fully operational.
- ✅ **Conflict Detection**: Verified 100% accuracy against Tree-sitter.
- ✅ **Code Generation**: Generates production-ready Nim parsers.
- ✅ **External Scanners**: Native C support via Nim's FFI.

## Testing Strategy

The project uses a regression suite of 51 real-world and edge-case grammars.
- Each test case includes a `grammar.js` and a `corpus.txt`.
- The CLI compiles the generated Nim code and runs it against the corpus.
- Nightly runs ensure continuous compatibility.

