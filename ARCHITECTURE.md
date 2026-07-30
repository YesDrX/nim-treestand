# Treestand Architecture

Treestand is a complete re-implementation of the Tree-sitter parser generator in Nim. This document outlines its internal structure.

## Overview

Treestand follows a pipeline architecture:
1. **JavaScript Execution**: `grammar.js` → JSON (via Bun or Node.js)
2. **Grammar Parsing**: JSON → `InputGrammar`
3. **Grammar Preparation**: `InputGrammar` → `SyntaxGrammar` + `LexicalGrammar`
4. **Table Building**: Grammars → `ParseTable` + `LexTable`
5. **Code Generation**: Tables → `parser.nim`

## Table Building (core of the speed refactor)

The table-building phase was rewritten (2026-07) to replace the previous
propagation-based (Pager's LALR) algorithm with a **direct LALR(1) construction**,
yielding 10-20× faster parser generation.

### Direct LALR(1) Construction

States are built via a work-list: each state's kernel is deduplicated by its
item-set *core* (CoreItems, which carry `inheritedPrecedence`). When a core is
revisited, lookahead sets are unioned and the state is re-queued, converging
to the LALR(1) fixpoint without a global propagation pass over the state graph.

Closures are computed in a **single pass** using a precomputed *transitive
closure cache* (`precomputeClosureCache`), mirroring tree-sitter's
`ParseItemSetBuilder::transitive_closure_additions`. The cache stores, for
each non-terminal, all items that must be added when expanding that symbol,
along with their spontaneous lookaheads and propagation flags.

### Two-Mode Precedence

- **Construction** (`fullPrecedence = false`, step-prec-only): only the
  precedence annotated on the step being expanded is kept. This yields few
  distinct precedence variants and compact state counts.
- **Fill phase** (`fullPrecedence = true`, full inheritance): precedence is
  also inherited from the enclosing production / parent item, matching the
  conflict-resolution behavior of the previous propagation-based pipeline.

### Compile-Time (VM) Compatibility

State-registration helpers (`registerStateLALR`, `sortedCores`) are module-level
procs with explicit `var` parameters so they work correctly in both runtime
and compile-time (nimvm / `tsGrammar` macro) paths.

## Performance

| Grammar | Before | After | Speedup |
|---------|--------|-------|---------|
| Python  | 15.7s  | ~0.7s | ~22×    |
| Go      | 4.3s   | ~0.4s | ~11×    |
| C       | 25.0s  | ~5.0s | ~5×     |
| JSON    | 0.05s  | 0.04s | ~1.3×   |

## Known Issues

- **Go grammar regression**: The transition to the additions-based
  single-pass closure produces different closure items for some Go states,
  causing conflict-resolution mismatches. A targeted follow-up will restore
  Pager's per-step closure for the construction phase.
- **`conflicting_precedence` / `reduce_repro`**: Pre-existing failures
  (parser generation should fail but succeeds). Unrelated to the refactor.

## Rust-to-Nim Mapping

| Tree-sitter (Rust) | Treestand (Nim) | Notes |
|---|---|---|
| `lib/src/prepare_grammar/` | `src/treestand/prepare_grammar.nim` | |
| `lib/src/build_tables/` | `src/treestand/build_tables.nim` | NFA, DFA, LALR(1) construction |
| `lib/src/generate/` | `src/treestand/codegen.nim` | |
| `cli/src/generate.rs` | `src/treestand/cli/generate.nim` | |
| `cli/src/test.rs` | `src/treestand/cli/test.nim` | |
| `lib/src/parser.c` | `src/treestand/parser_runtime.nim` | |
