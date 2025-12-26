# Advanced Usage

## Resolving LR Conflicts

Treestand generates GLR (Generalized LR) parsers, which can handle non-deterministic parsing by splitting the stack. However, it is efficient to minimize conflicts (Shift/Reduce or Reduce/Reduce) where possible.

### Shift/Reduce Conflicts
Usually occur when the parser doesn't know whether to shift a token or reduce a rule.
*   **Fix**: Use `prec_left` or `prec_right` to specify associativity.
*   **Fix**: Use `prec(N, ...)` to specify that one rule binds tighter than another.

### Dynamic Precedence
Sometimes static precedence isn't enough, especially when context matters. Treestand supports **Dynamic Precedence**.

```nim
# Assign a dynamic score to a rule
# At runtime, if multiple parse branches exist, the one with higher cumulative dynamic precedence wins.
Variable("rule_A", vtNamed, prec_dynamic(1, ...))
```

This is useful for resolving ambiguity in C++ (declarations vs expressions) or complex method invocations.

## Debugging

### Inspecting Tables
You can inspect the generated `parser.nim` to see the `parseTableEntries`. While dense, they map states to actions.

### Trace Logging
The generated parser includes debug hooks. (Integration varies, but typically you can enable debug flags during generation or compilation).

## Customizing the Lexer
Treestand uses a DFA-based lexer generated from Regex rules.
*   **Keyword Extraction**: If strict keywords are overlapping with identifiers, ensure `wordToken` is set correctly in `prepareGrammar` (automating keyword priority).
*   **Reserved Words**: Use `reserved(...)` in DSL to explicitly handle reserved identifiers.

## Contributing
Treestand is open source! Explore the `src/` directory to understand the pipeline:
1.  `rules.nim` / `dsl.nim`: Grammar definition.
2.  `prepare_grammar.nim`: Simplification.
3.  `build_tables.nim`: LR(1) Item set construction.
4.  `codegen.nim`: Nim code emission.
