# Using `buildGrammar` Macro

The `buildGrammar` macro is Treestand's most powerful feature for defining grammars entirely in pure Nim, without any JavaScript dependencies. It allows you to define your grammar using Nim's DSL and generates a complete parser at compile-time.

## Overview

Unlike `importGrammar` which requires a `grammar.js` file, `buildGrammar` lets you define grammars using pure Nim code. You write a function that returns an `InputGrammar` object, and `buildGrammar` generates a parser from it at compile-time.

### Meta-Macro Pattern

`buildGrammar` is a **meta-macro** - it generates another macro that performs the actual parser generation:

1. You define a function returning `InputGrammar`
2. `buildGrammar` generates a temporary macro that calls your function
3. The temporary macro builds the parser at compile-time
4. The parser code is injected into your module

All of this happens transparently during compilation!

## Basic Usage

```nim
import treestand
import std/options

# Define your grammar using pure Nim
proc createMathGrammar(): InputGrammar =
  InputGrammar(
    name: "math",
    variables: @[
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),
      Variable(name: "expression", kind: vtNamed, rule: sym("number"))
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )

#Build the parser at compile-time
buildGrammar(createMathGrammar)

# Use the generated parser
when isMainModule:
  let tree = parseMath("123")
  echo tree
```

## DSL Functions Reference

Treestand provides a complete DSL for defining grammars. All functions are available from `treestand/dsl`.

### Structure Functions

#### `sym(name: string)` - Symbol Reference
References another rule by name.
```nim
Variable(name: "expression", kind: vtNamed, rule: sym("number"))
```

#### `seq(items: varargs[Rule])` - Sequence
A sequence of elements that must appear in order.
```nim
# Matches: number + number
rule: seq(sym("number"), str("+"), sym("number"))
```

#### `choice(items: varargs[Rule])` - Choice/Alternatives
Matches one of several alternatives (OR).
```nim
# Matches either number OR identifier
rule: choice(sym("number"), sym("identifier"))
```

### Repetition Functions

#### `rep(item: Rule)` - One or More
Matches one or more occurrences.
```nim
# Matches: 1, 12, 123, etc.
rule: rep(sym("digit"))
```

#### `rep0(item: Rule)` - Zero or More
Matches zero or more occurrences.
```nim
# Matches: "", "a", "aa", "aaa", etc.
rule: rep0(str("a"))
```

#### `opt(item: Rule)` - Optional
Matches zero or one occurrence.
```nim
# Optiona l minus sign
rule: seq(opt(str("-")), sym("number"))
```

### Lexical Functions

#### `token(rule: Rule)` - Lexical Token
Marks a rule as a lexical token (handled by the lexer).
```nim
Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+")))
```

#### `patt(regex: string)` - Pattern
Creates a regular expression pattern.
```nim
# Match digits
rule: token(patt("\\d+"))

# Match identifiers
rule: token(patt("[a-zA-Z_][a-zA-Z0-9_]*"))
```

#### `str(text: string)` - Literal String
Matches an exact string.
```nim
rule: str("+")  # Matches the "+" character
```

### Precedence Functions

#### `prec_left(n: int, rule: Rule)` - Left Associativity
Specifies left associativity with precedence level `n`.
```nim
# 1 + 2 + 3 parsed as (1 + 2) + 3
rule: prec_left(1, seq(sym("expr"), str("+"), sym("expr")))
```

#### `prec_right(n: int, rule: Rule)` - Right Associativity
Specifies right associativity with precedence level `n`.
```nim
# 2 ^ 3 ^ 4 parsed as 2 ^ (3 ^ 4)
rule: prec_right(2, seq(sym("expr"), str("^"), sym("expr")))
```

#### `prec(n: int, rule: Rule)` - Precedence
Specifies precedence without associativity.
```nim
rule: prec(3, seq(sym("expr"), str("*"), sym("expr")))
```

#### `prec_dynamic(n: int, rule: Rule)` - Dynamic Precedence
Specifies dynamic precedence (resolved at parse-time).
```nim
rule: prec_dynamic(1, sym("declaration"))
```

## Complete Example: Math Expression Parser

Here's a complete example with proper precedence and associativity:

```nim
import treestand
import std/options

proc createMathGrammar(): InputGrammar =
  ## Mathematical expression grammar with proper operator precedence
  InputGrammar(
    name: "math",
    variables: @[
      # Start rule: one or more expressions
      Variable(name: "program", kind: vtNamed, 
               rule: rep(sym("expression"))),
      
      # Expression can be a number or binary operation
      Variable(name: "expression", kind: vtNamed,
               rule: choice(sym("number"), sym("binary_op"))),
      
      # Binary operation with left associativity
      # Precedence 1: + and - (lowest)
      # Precedence 2: * and / (higher)
      Variable(name: "binary_op", kind: vtNamed,
               rule: prec_left(1, seq(
                 sym("expression"),
                 sym("op"),
                 sym("expression")
               ))),
      
      # Number: one or more digits
      Variable(name: "number", kind: vtNamed,
               rule: token(patt("\\d+"))),
      
      # Operator: +, -, *, /
      Variable(name: "op", kind: vtNamed,
               rule: token(patt("[+\\-*/]")))
    ],
    
    # Whitespace is skipped automatically
    extraSymbols: @[token(patt("\\s+"))]
  )

buildGrammar(createMathGrammar)

when isMainModule:
  let input = "1 + 2 * 3"
  var parser = newParser(input)
  let tree = parser.parse()
  echo tree
```

Output:
```
program
  program_repeat1
    expression
      binary_op
        expression
          binary_op
            expression
              number "1"
            op "+"
            expression
              number "2"
        op "*"
        expression
          number "3"
```

## Grammar Structure

### InputGrammar Type

```nim
type InputGrammar* = object
  name*: string                      # Grammar name (used for function naming)
  variables*: seq[Variable]          # Grammar rules
  extraSymbols*: seq[Rule]          # Tokens to skip (typically whitespace)
  externalTokens*: seq[string]      # External scanner tokens (if any)
  conflicts*: seq[seq[string]]      # Expected conflicts (optional)
  precedences*: seq[seq[string]]    # Precedence declarations (optional)
  wordToken*: Option[string]        # Word token for keywords (optional)
```

### Variable Type

```nim
type Variable* = object
  name*: string        # Rule name
  kind*: VariableType  # vtNamed, vtAnonymous, vtHidden, or vtAuxiliary
  rule*: Rule          # The rule definition
```

**Variable Kinds:**
- `vtNamed` - Normal rule, appears in parse tree
- `vtAnonymous` - Hidden from tree but rule appears
- `vtHidden`- Completely hidden from parse tree
- `vtAuxiliary` - Helper rule

## Advanced Features

### Multiple Precedence Levels

```nim
proc createCalcGrammar(): InputGrammar =
  InputGrammar(
    name: "calc",
    variables: @[
      Variable(name: "expr", kind: vtNamed,
               rule: choice(
                 # Higher precedence (multiplication/division)
                 prec_left(2, seq(sym("expr"), patt("[*/]"), sym("expr"))),
                 # Lower precedence (addition/subtraction)
                 prec_left(1, seq(sym("expr"), patt("[+\\-]"), sym("expr"))),
                 sym("number")
               )),
      Variable(name: "number", kind: vtNamed,
               rule: token(patt("\\d+")))
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )
```

### Grouping with Parentheses

```nim
proc createParenGrammar(): InputGrammar =
  InputGrammar(
    name: "paren",
    variables: @[
      Variable(name: "expr", kind: vtNamed,
               rule: choice(
                 # Parenthesized expression
                 seq(str("("), sym("expr"), str(")")),
                 prec_left(1, seq(sym("expr"), str("+"), sym("expr"))),
                 sym("number")
               )),
      Variable(name: "number", kind: vtNamed,
               rule: token(patt("\\d+")))
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )

buildGrammar(createParenGrammar)
```

### Lists and Separators

```nim
# Comma-separated list
Variable(name: "list", kind: vtNamed,
         rule: seq(
           sym("item"),
           rep0(seq(str(","), sym("item")))
         ))
```

## Advantages Over Other Approaches

### vs. `importGrammar`

**buildGrammar advantages:**
- ✅ **No JavaScript**: Zero JavaScript dependencies
- ✅ **Type Safety**: Full Nim type checking
- ✅ **IDE Support**: Autocomplete, goto definition, error checking
- ✅ **Refactoring**: Easy to refactor and reuse grammar components
- ✅ **Single File**: Grammar and code in one place

**importGrammar advantages:**
- ✅ **Tree-sitter Compatibility**: Use existing Tree-sitter grammars
- ✅ **External Scanners**: Support for C scanner files
- ✅ **Community Grammars**: Leverage existing grammars

### vs. Manual Generation

**buildGrammar:**
- ✅ **No Files**: No intermediate parser.nim files
- ✅ **Compile-Time**: All generation at compile-time
- ✅ **Type-Safe DSL**: Grammar errors caught by compiler

## Best Practices

### 1. Organize Complex Grammars

```nim
# Helper functions for common patterns
proc identifier(): Rule = token(patt("[a-zA-Z_][a-zA-Z0-9_]*"))
proc whitespace(): Rule = token(patt("\\s+"))

proc createMyGrammar(): InputGrammar =
  InputGrammar(
    name: "my lang",
    variables: @[
      Variable(name: "program", kind: vtNamed, rule: rep(sym("statement"))),
      Variable(name: "identifier", kind: vtNamed, rule: identifier())
    ],
    extraSymbols: @[whitespace()]
  )
```

### 2. Use Descriptive Names

```nim
# Good: descriptive names
Variable(name: "function_declaration", kind: vtNamed, ...)
Variable(name: "parameter_list", kind: vtNamed, ...)

# Avoid: cryptic abbreviations
Variable(name: "fn_decl", kind: vtNamed, ...)
Variable(name: "param_lst", kind: vtNamed, ...)
```

### 3. Comment Your Grammar

```nim
proc createGrammar(): InputGrammar =
  InputGrammar(
    name: "mylang",
    variables: @[
      # A program consists of zero or more statements
      Variable(name: "program", kind: vtNamed, rule: rep0(sym("statement"))),
      
      # Statements can be declarations or expressions
      Variable(name: "statement", kind: vtNamed,
               rule: choice(sym("declaration"), sym("expression")))
    ],
    extraSymbols: @[]
  )
```

### 4. Test Incrementally

Build your grammar incrementally, testing as you go:

```nim
# Start simple
proc v1(): InputGrammar =
  InputGrammar(name: "test", variables: @[
    Variable(name: "program", kind: vtNamed, rule: sym("number")),
    Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+")))
  ], extraSymbols: @[])

buildGrammar(v1)

# Test parsing
when isMainModule:
  echo parseTest("123")
  # Then expand...
```

## Common Patterns

### Optional Trailing Comma

```nim
# list: item ("," item)* ","?
Variable(name: "list", kind: vtNamed,
         rule: seq(
           sym("item"),
           rep0(seq(str(","), sym("item"))),
           opt(str(","))
         ))
```

### Binary Operators with Precedence

```nim
proc createExprGrammar(): InputGrammar =
  InputGrammar(
    name: "expr",
    variables: @[
      Variable(name: "expr", kind: vtNamed,
               rule: choice(
                 # Multiplication (highest precedence)
                 prec_left(3, seq(sym("expr"), str("*"), sym("expr"))),
                 # Addition
                 prec_left(2, seq(sym("expr"), str("+"), sym("expr"))),
                 # Comparison (lowest precedence)
                 prec_left(1, seq(sym("expr"), str("=="), sym("expr"))),
                 sym("number")
               )),
      Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+")))
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )
```

### Keywords vs Identifiers

```nim
InputGrammar(
  name: "lang",
  variables: @[
    # Keywords
    Variable(name: "if_keyword", kind: vtNamed, rule: token(str("if"))),
    Variable(name: "else_keyword", kind: vtNamed, rule: token(str("else"))),
    
    # Identifiers (won't match keywords due to longest-match)
    Variable(name: "identifier", kind: vtNamed,
             rule: token(patt("[a-zA-Z_][a-zA-Z0-9_]*")))
  ],
  wordToken: some("identifier")  # Helps with keyword handling
)
```

## Debugging

### Compilation Messages

During compilation, `buildGrammar` shows the generated macro code:

```
macro buildGrammarImpl(): untyped =
  let inputGrammar = createMathGrammar()
  ...
```

This helps you understand what's happening and debug issues.

### Parse Tree Inspection

Use the generated parse tree to understand how your grammar works:

```nim
let tree = parseMath("1 + 2 * 3")
echo tree  # Shows full tree structure
```

### Conflict Resolution

If you have shift/reduce or reduce/reduce conflicts, the compiler will report them during table generation:

```
[Treestand] Warning: Shift/Reduce conflict ...
```

Use precedence directives to resolve them.

## Performance Considerations

- **Compilation Time**: Complex grammars increase compile time (usually a few seconds)
- **Runtime Performance**: Zero impact - generated parser is as fast as manually written ones
- **Binary Size**: Each grammar adds to binary size (similar to other approaches)

## Limitations

1. **No External Scanners**: Can't use C scanner files (use `importGrammar` for those)
2. **Compile-Time Only**: Grammar must be defined at compile-time
3. **Nim Dependencies**: Requires Nim compilation (unlike standalone parsers)

For maximum flexibility with external scanners, use `importGrammar`. For pure Nim development, `buildGrammar` is ideal.

## See Also

- [Import Grammar](import_grammar.md) - Importing Tree-sitter grammars
- [Using DSL](using_dsl.md) - Detailed DSL function reference
- [Advanced Usage](advanced_usage.md) - Conflict resolution and debugging
