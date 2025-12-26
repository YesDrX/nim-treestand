# Querying

Tree-stand provides a [Tree-sitter](https://tree-sitter.github.io/tree-sitter/using-parsers#pattern-matching-with-queries) compatible query engine. This allows you to search for patterns in your syntax tree using a specialized S-expression syntax.

## Basic Usage

To use the query engine, import `treestand` (check 03_query example)

```nim
import treestand

# 1. Parse your source code
import parser as myParser
let parser = myParser.newParser("YOUR_SOURCE_CODE")
let tree = parser.parse()

# 2. Create a Query
let q = newQuery("""
  (function_definition
    (identifier) @func.name
    (block) @func.body)
""")

# 3. Define a symbol lookup (maps internal Symbol IDs to names/kinds)
proc mySymbolName(s: Symbol): string =
  # ... implementation depends on your parser ...
  return "function_definition"

# 4. Execute the query
for match in exec(q, tree, mySymbolName):
  echo "Found match!"
  for cap in match.captures:
    echo "  Capture: ", cap.name, " -> ", cap.node.token.text
```

## Query Syntax

The query syntax is based on S-expressions.

### Node Matching
Match a node by its type name:
```sexp
(identifier)
```

### Nesting
Match a node and its children:
```sexp
(binary_expression
  (identifier)
  (number))
```

### Captures
Capture a node for later inspection using `@name`:
```sexp
(function_definition
  (identifier) @function_name)
```

### Wildcards
Match any node using `_`:
```sexp
(call_expression
  (identifier)
  (_))
```

### Anchors
Force two nodes to be immediate siblings using `.`:
```sexp
(return_statement . (semicolon))
```

### Fields
Match a specific field of a node (syntax supported, semantic check depends on parser):
```sexp
(binary_expression
  left: (identifier))
```

## Future Work

The current Query Engine implementation is a solid foundation but has several areas planned for improvement:

1.  **Field Validations**: Currently, field names in queries (e.g., `left:`) are parsed but not strictly enforced during matching because the standard `ParseNode` does not explicitly store field labels for every child. Future updates will integrate field mapping into the runtime lookup.
2.  **Negation**: Support for `!` to negate a match (e.g., `(identifier) !@exclude`).
3.  **Predicates**: Support for standard Tree-sitter predicates like `#eq?`, `#match?` to filter results based on text content.
4.  **Optimized Cursor**: The current `exec` iterator performs a simple depth-first search. A cursor-based approach (similar to Tree-sitter's `QueryCursor`) that properly tracks position and ignores irrelevant subtrees would improve performance for large files.
5.  **Quantifiers**: Full support for `*` (zero-or-more), `+` (one-or-more), and `?` (optional) suffixes on nodes in the query pattern itself (distinct from the grammar definition).

