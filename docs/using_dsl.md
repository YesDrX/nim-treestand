# Using the Treestand DSL

Treestand provides a powerful Domain Specific Language (DSL) in Nim to define grammars programmatically. This approach gives you full power of the host language (macros, loops, variables) to construct complex grammars.

## Introduction

Treestand offers two ways to define grammars:
1.  **Macro DSL (`tsGrammar`)**: A concise, operator-based syntax inspired by PEG/npeg. (Recommended)
2.  **Procedural DSL**: A set of helper functions (`seq`, `choice`, `rep`, etc.) used to build `InputGrammar` objects manually.

This guide covers both, starting with the recommended macro DSL.

## Macro DSL (`tsGrammar`)

The `tsGrammar` macro allows you to define grammars using a clean, readable syntax.

### Basic Syntax

```nim
import treestand

tsGrammar "my_lang":
  # Rule Assignment
  program     <- +stmt

  # Sequence (*) and Choice (|)
  stmt        <- assign * semi
  
  # Repetition
  # +rule  -> One or more
  # *rule  -> Zero or more
  # ?rule  -> Optional
  assign      <- (variable: identifier) * eq * (value: expr) # Named fields by (fld : rule) format
  expr        <- identifier | number | external_token # external_token is a token handled by an external scanner (C function), but not implemented in tsGrammar yet
  
  # Lexical Tokens
  # Use token() wrapper for lexical rules
  # String literals and regex patterns are auto-wrapped with str() or patt()
  identifier  <- token(re"\w+")
  number      <- token(re"\\d+")
  eq          <- token("=")
  semi        <- token(";")
  
  # Configuration·
  extras      = token(re"\s+")
  # word        = "identifier"

when isMainModule:
  echo parseMyLang("a = 1; b=a;")
```

### Operators & Mapping

The macro translates operators into procedural DSL calls:

| Operator | Syntax | Equivalent Function | Description |
| :--- | :--- | :--- | :--- |
| **Assignment** | `name <- rule` | `Variable(name, ...)` | Defines a rule. |
| **Sequence** | `a * b` | `seq(a, b)` | Matches `a` then `b`. |
| **Choice** | `a \| b` | `choice(a, b)` | Matches `a` OR `b`. |
| **One-or-More** | `+a` | `rep(a)` | Matches `a` at least once. |
| **Zero-or-More** | `*a` | `opt(rep(a))` | Matches `a` zero or more times. |
| **Optional** | `?a` | `opt(a)` | Matches `a` zero or one time. |
| **Precedence** | `rule ^ N` | `prec_left(N, rule)` | Sets precedence level `N`. |
| **Named Field** | `(name: rule)` | `field("name", rule)` | Assigns a field name to the node. |
| **Set/Keywords** | `{"a", "b"}` | `choice(str("a"), str("b"))` | Matches one of the string literals. |

### Configuration Properties

You can set `InputGrammar` properties using `field = value` syntax within the macro block:

-   `extras = rule`: Define tokens to skip (e.g., whitespace).
-   `conflicts = @[...]`: Document expected conflicts.
-   `inline = @["rule"]`: Rules to inline.
-   `supertypes = @["rule"]`: Rules to treat as supertypes.
-   `word = "token_name"`: Token to use for keyword extraction optimization.
-   `scanner = "path"`: Placeholder for external scanner path (handled by CLI usually).

### External Tokens

To define a token handled by an external scanner (C function):

```nim
indent <- external_token
```

## Procedural DSL (Low-Level)

The procedural DSL involves manually constructing `InputGrammar` objects using helper functions. This is useful for programmatic generation or dynamic grammars.

### Core DSL Functions

Import `treestand/dsl` to access these helpers.

### Basic Rules

| Function | Description | Example |
|----------|-------------|---------|
| `str("s")` | Literal string match. | `str("class")` |
| `patt("r")` | Regex pattern. | `patt("[a-zA-Z_]\w*")` |
| `sym("name")` | Reference to another rule. | `sym("statement")` |

### Combinators

| Function | Description | Example |
|----------|-------------|---------|
| `seq(...)` | Sequence (concatenation). | `seq(str("if"), sym("expr"), str(":"))` |
| `choice(...)` | Ordered choice (alternatives). | `choice(sym("if_stmt"), sym("while_stmt"))` |
| `rep(rule)` | Repetition (one or more). | `rep(sym("item"))` |
| `opt(rule)` | Optional (zero or one). | `opt(str("pub"))` |

### Metadata & Tokens

| Function | Description | Example |
|----------|-------------|---------|
| `token(rule)` | Mark rule as a lexical token. This collapses the internal structure into a single leaf node. | `token(seq(patt("\d+"), opt(str("."))))` |

### Precedence & Associativity

Handling ambiguity is crucial for parsers.

| Function | Description | Example |
|----------|-------------|---------|
| `prec(n, rule)` | Static precedence level `n`. | `prec(2, sym("mult_op"))` |
| `prec_left(n, rule)` | Left associativity. | `1 + 2 + 3` -> `(1 + 2) + 3` |
| `prec_right(n, rule)` | Right associativity. | `x = y = z` -> `x = (y = z)` |

## Create a Grammar using DSL
```nim
import treestand

proc createMathGrammar(): InputGrammar =
  ## Defines a simple mathematical expression grammar.
  ## This grammar supports basic arithmetic operations (+, -, *, /) on integers.
  InputGrammar(
    name: "math",
    variables: @[
      # 'program' is the start rule. It consists of one or more expressions.
      # `rep` is a DSL helper for repetition (one or more).
      # `sym` refers to another rule by name.
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),

      # 'expression' can be either a number or a binary operation.
      # `choice` represents alternatives (OR).
      Variable(name: "expression", kind: vtNamed, rule: choice(sym("number"), sym("binary_op"))),

      # 'binary_op' defines the structure of an operation: expression <op> expression.
      # `prec_left(1, ...)` specifies left associativity with precedence level 1.
      # This ensures operations like 1 + 2 + 3 are parsed as (1 + 2) + 3.
      # `seq` represents a sequence of elements.
      Variable(name: "binary_op", kind: vtNamed, rule: prec_left(1, seq(sym("expression"), sym("op"), sym("expression")))),

      # 'number' is a lexical token matching digits.
      # `token` marks this rule as a lexical unit (handled by the lexer).
      # `patt` creates a regex pattern.
      Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+"))),

      # 'op' is a lexical token matching operator characters.
      Variable(name: "op", kind: vtNamed, rule: token(patt("[+\\-*/]")))
    ],
    
    # 'extraSymbols' are tokens that can appear anywhere between other tokens, usually whitespace or comments.
    # They are automatically skipped by the parser but consumed by the lexer.
    extraSymbols: @[ token(patt("\\s+")) ]
  )

when isMainModule:
  echo "Generating parser for 'math' grammar..."
  let g = createMathGrammar()

  generateParser(g, "myparser.nim")

  echo "Parser generated to myparser.nim"
```

## Using the generated Parser

Once you have generated your `myparser.nim` (see Getting Started), you can interact with it.

### Parsing

```nim
import myparser

var parser = newParser("source code")
let rootNode = parser.parse()
```

### Traversing the Tree

The `ParseNode` object provides runtime access to the tree.

```nim
echo kind(rootNode)   # e.g. "program"
echo rootNode.startPos # Byte offset

for child in children(rootNode):
  echo kind(child)
```

### Editing (Incremental Parsing)

Treestand supports basic tree editing APIs to prepare for incremental parsing support.

```nim
# Example: Insert text into the source
# Note: Full incremental reparsing is an advanced topic
let newSource = "1 + 2"
# ... edit logic ...
```

### Querying

Treestand supports a Tree-sitter compatible query language (S-expressions) for pattern matching against the tree.

```nim
# Conceptual usage
let query = """
(binary_op
  left: (number) @left
  operator: (op) @op
  right: (number) @right)
"""
# match(rootNode, query) ...
```
