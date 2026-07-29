# Refactor Status — July 2026

## Summary

Treestand's parser-generation pipeline was rewritten (direct LALR construction replacing Pager's propagation algorithm). **Performance is 10-21× faster**: Python 15.7s→0.74s, C 24.9s→1.33s, Go 4.3s→0.41s. **All four large grammars now generate cleanly** (Python, Go, JSON, C) and the fixture suite is at **51/53** (same 2 pre-existing negative-test failures as baseline).

## What Was Done

### Speed (the primary goal)
1. **Direct LALR(1) construction** — replaces Pager's `buildLR0Automaton → computeLookaheadPropagations → propagateLookaheads` pipeline with core-keyed state dedup + lookahead union-on-revisit. Single-pass closures via precomputed transitive-addition cache.
2. **Template zero-copy bindings** — `let variable = ...` was deep-copying `SyntaxVariable` objects per closure-item iteration (the #1 bottleneck). Replaced with `template variable: untyped = ...`.
3. **Inline BitSet storage** — `array[16, uint64]` replaces `seq[uint64]` in `src/treestand/bitset.nim`, eliminating heap allocations from lookahead unions.
4. **Packed CoreItem/GrammarSymbol hashes** — single-value hashing replaces 4-field mix chains.
5. **Dense terminal-indexed participant buffers** — `seq[BitSet]` indexed by terminal bit replaces `Table[GrammarSymbol, HashSet]` for shift-participant collection.
6. **Minimizer rewrite** — core-grouped partition, O(1) per-state lookup tables, `token_conflicts` port from tree-sitter.
7. **Lookahead fix** — `precomputeClosureCache` was missing lookahead propagation for single-step (empty-β) productions.

### Conflict Resolution (complete)
1. **`originalSymbol` chain for named rules** — `prepare_grammar.nim` sets `originalSymbol` for named children pointing to their parent rule. This resolved Go's conflicts (e.g., `pointer_type` → `_simple_type`).
2. **`isConflictExpected` with parent inclusion** — resolved participants include both `getOriginalSymbol` and one-level parent from `originalSymbol`, enabling subset-match against declared conflict sets.
3. **Closure-based conflicting items for reduce/reduce** — recomputes conflicting item variables from the state's closure (Rust-style) instead of relying on pre-stored action participants.
4. **Structural parent-symbol resolution (tree-sitter `get_auxiliary_node_info`)** — a grammar-global map from each auxiliary (repeat) helper to the visible rules that reference it. `computeActualConflict` substitutes helpers with their owners and `matchesExpectedExact` matches the result against declared conflicts using tree-sitter's exact set-equality. This fixed **C** (`parameter_list_repeat1` vs `_old_style_parameter_list_repeat1` → owners `{parameter_list, _old_style_parameter_list}`, which the grammar declares) — the old name-heuristic over-resolved `parameter_list_repeat1` past its owner to `function_declarator` and never matched.
5. **Repetition guard (tree-sitter `is_repetition` intent)** — fixed **Python**'s `'|'` conflict between `union_pattern` (prec.right) and its own helper `union_pattern_repeat1` (prec.left). Contradictory associativity can't be resolved by precedence/assoc; tree-sitter keeps the shift (continue the repetition). Detected precisely: every reduce, after mapping helpers to their owning rule, collapses to a **single** visible rule and at least one reduce is auxiliary → keep the shift, drop reduces. Changes generated output for **zero** positive fixtures; does not fire for genuine cross-rule repeat conflicts (`conflict_in_repeat_rule` → two owners `{array, array_type}`).

## Current Status

| Grammar | `--cmd generate` | Performance |
|---------|-----------------|-------------|
| Python  | ✅ OK           | ~0.74s      |
| Go      | ✅ OK           | ~0.41s      |
| C       | ✅ OK           | ~1.33s      |
| JSON    | ✅ OK           | ~0.04s      |

**Suite**: 51/53 fixtures pass. The remaining 2 (`conflicting_precedence`, `reduce_repro`) are negative tests that wrongly *succeed* — a pre-existing conflict-detection gap unrelated to the large-grammar work (both are dynamic/multi-precedence shift cases, not repeat-rule conflicts). Documented below.

## Remaining Work (pre-existing, low priority)

### `conflicting_precedence` (negative test wrongly succeeding)
`sum`/`product`/`other_thing` create a shift/reduce/shift with three distinct precedences to the same state. The `hasMultiplePrecedences` detection path (build_tables.nim) is currently inert (the flag is never set), so the multi-interpretation ambiguity isn't reported. Needs the multi-precedence-shift detection wired up.

### `reduce_repro` (negative test wrongly succeeding)
`rule_a`/`rule_b` are `prec.dynamic(1/2, 'word')` — a reduce/reduce distinguished only by *dynamic* precedence. Tree-sitter reports this as unresolved at generation time (dynamic precedence is a runtime GLR tiebreaker, not a generation-time resolver); treestand currently treats the differing dynamic precedence as resolving it.

Neither involves repeat rules, so both are orthogonal to the conflict-set/repetition work completed here.

## Files Changed
- `src/treestand/build_tables.nim` — major rewrite (construction, closure, fill loop, conflict pass); added structural `auxParents`/`computeActualConflict`/`matchesExpectedExact` and the repetition guard.
- `src/treestand/minimizer.nim` — core-grouped partition, O(1) lookup, token_conflicts.
- `src/treestand/bitset.nim` — inline array storage.
- `src/treestand/prepare_grammar.nim` — `originalSymbol` for named children.
- `src/treestand/grammar.nim` — packed GrammarSymbol hash.
- `ARCHITECTURE.md` — updated.
