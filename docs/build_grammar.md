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
              number
                number "1" [0-1]
            op
              op "+" [2-3]
            expression
              number
                number "2" [4-5]
        op
          op "*" [6-7]
        expression
          number
            number "3" [8-9]
```

## Grammar Structure

### InputGrammar Type

```nim
  InputGrammar* = object
    ## The raw grammar definition provided by the user (or DSL).
    ## This is the input to the `prepareGrammar` phase.
    name*: string                                ## Examples: "json", "python".
    variables*: seq[Variable]                    ## List of all defined rules.
    extraSymbols*: seq[Rule]                     ## Whitespace, comments to be skipped.
    expectedConflicts*: seq[seq[string]]         ## Documented LR conflicts to suppress warnings.
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    externalTokens*: seq[Rule]                   ## Tokens handled by external scanner.
    variablesToInline*: seq[string]              ## Rules to inline into their callers.
    supertypeSymbols*: seq[string]               ## Rules grouped as supertypes (for query API).
    wordToken*: Option[string]                   ## Keyword tokenizer optimization.
    reservedWords*: seq[ReservedWordContext[Rule]]

  VariableType* = enum
    ## Classifies the role of a grammar variable (Rule).
    vtHidden     ## Internal rule, not exposed in the CST (prefixed with `_`).
    vtAuxiliary  ## Generated helper rule (e.g. for repetition or sequences).
    vtAnonymous  ## String literal or pattern (e.g. "if", "\d+").
    vtNamed      ## Standard named rule defined in the grammar.

  Variable* = object
    ## Represents a high-level rule definition from the user's input grammar.
    name*: string
    kind*: VariableType
    rule*: Rule

  RuleKind* = enum
    ## Enumeration of all possible AST node types for the grammar DSL.
    rkBlank        ## Empty rule.
    rkString       ## Literal string match (e.g. "if").
    rkPattern      ## Regex pattern (e.g. "\d+").
    rkNamedSymbol  ## Reference to another named rule (variable).
    rkSymbol       ## Resolved symbol reference (internal use).
    rkChoice       ## Alternative options (OR / |).
    rkMetadata     ## Wraps another rule with metadata (prec, assoc, token).
    rkRepeat       ## Repetition (zero-or-more, one-or-more).
    rkSeq          ## Sequence of rules (concatenation).
    rkReserved     ## Reserved word handling.

  Rule* = object
    ## The core recursive data structure representing a grammar rule expression.
    ## This AST is built via the DSL and processed to generate the parser.
    case kind*: RuleKind
    of rkBlank:
      discard
    of rkString:
      stringValue*: string
    of rkPattern:
      patternValue*: string
      patternFlags*: string
    of rkNamedSymbol:
      symbolName*: string
    of rkSymbol:
      symbol*: GrammarSymbol
    of rkChoice:
      choiceMembers*: seq[Rule]
    of rkMetadata:
      metadataParams*: MetadataParams
      metadataRule*: ref Rule
    of rkRepeat:
      repeatContent*: ref Rule
    of rkSeq:
      seqMembers*: seq[Rule]
    of rkReserved:
      reservedRule*: ref Rule
      reservedContextName*: string

  MetadataParams* = object
    ## Stores metadata associated with a rule, affecting parsing behavior.
    precedence*: Precedence              ## Static precedence.
    dynamicPrecedence*: int32            ## Dynamic precedence (runtime conflict resolution).
    associativity*: Option[GrammarAssociativity] ## Left or Right associativity.
    isToken*: bool                       ## Marks a rule as a lexical token (atomic).
    isMainToken*: bool                   ## Internal flag for main lexer tokens.
    alias*: Option[Alias]                ## Alias for the resulting node.
    fieldName*: Option[string]           ## Field name for structure access (e.g. `left:`).

  GrammarSymbolType* {.size: 2.} = enum
    ## Distinguishes between different kinds of symbols in the grammar.
    stExternal              ## Symbol managed by an external scanner (C/C++).
    stEnd                   ## End of input (EOF) or end of sequence.
    stEndOfNonTerminalExtra ## Special internal symbol for GLR processing.
    stTerminal              ## A lexical token (leaf node e.g., string literal, regex).
    stNonTerminal           ## A syntactic rule composed of other symbols.

  GrammarSymbol* = object
    ## Represents a unique symbol in the grammar, identified by type and index.
    ## This is the build-time representation, distinct from `parser_types.Symbol`.
    kind*: GrammarSymbolType
    index*: uint16  ## Index into the respective symbol list (terminals, non-terminals, etc.).

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
- [TS Macros](ts_macros.html) - Tree-sitter macros: importGrammar, tsGrammar, buildGrammar
- [Using DSL](using_dsl.md) - Detailed DSL function reference
- [Advanced Usage](advanced_usage.md) - Conflict resolution and debugging
