# Example 06: Using `buildGrammar` Macro

This example demonstrates how to use Treestand's `buildGrammar` macro to define a grammar entirely in pure Nim and generate a parser at compile-time, without any JavaScript dependencies.

## What This Example Does

This example shows how to:
1. Define a mathematical expression grammar using pure Nim DSL
2. Build a parser at compile-time using `buildGrammar`
3. Parse arithmetic expressions with! proper operator precedence
4. Display the parse tree

## Key Features

### Pure Nim Grammar Definition

Unlike `importGrammar` which requires a `grammar.js` file, `buildGrammar` lets you define grammars using pure Nim:

```nim
proc createMathGrammar(): InputGrammar =
  InputGrammar(
    name: "math",
    variables: @[
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),
      Variable(name: "expression", kind: vtNamed, rule: choice(sym("number"), sym("binary_op"))),
      // ... more rules
    ],
    extraSymbols: @[token(patt("\\s+"))]
  )
```

### Compile-Time Parser Generation

The `buildGrammar` macro:
- Executes your grammar function at compile-time
- Builds the parsing tables
- Generates optimized parser code
- Injects it directly into your module

All of this happens during compilation, with **zero runtime overhead**.

### Type-Safe DSL

The grammar is defined using type-safe Nim code, so you get:
- Full IDE autocomplete
- Type checking at compile-time
- Error messages from the Nim compiler
- Easy refactoring and code reuse

## Running the Example

```bash
cd examples/06_buildGrammar
nim r main.nim
```

## Expected Output

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

Notice how the multiplication (`2 * 3`) is parsed first due to proper precedence handling.

## Code Walkthrough

### 1. Define the Grammar Function

```nim
proc createMathGrammar(): InputGrammar =
  InputGrammar(
    name: "math",
    variables: @[
      # Start rule
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),
      
      # Expression alternatives
      Variable(name: "expression", kind: vtNamed,
               rule: choice(sym("number"), sym("binary_op"))),
      
      # Binary operation with precedence
      Variable(name: "binary_op", kind: vtNamed,
               rule: prec_left(1, seq(
                 sym("expression"),
                 sym("op"),
                 sym("expression")
               ))),
      
      # Lexical tokens
      Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+"))),
      Variable(name: "op", kind: vtNamed, rule: token(patt("[+\\-*/]")))
    ],
    
    # Whitespace is skipped automatically
    extraSymbols: @[token(patt("\\s+"))]
  )
```

**What's happening:**
- `rep(sym("expression"))` - One or more expressions
- `choice(...)` - Alternatives (number OR binary_op)
- `prec_left(1, ...)` - Left associativity with precedence 1
- `seq(...)` - Sequence of elements
- `token(patt(...))` - Lexical token with regex pattern
- `extraSymbols` - Tokens to skip (whitespace)

### 2. Build the Parser

```nim
buildGrammar(createMathGrammar)
```

**What happens here:**
- `buildGrammar` is a meta-macro that generates another macro
- The generated macro calls `createMathGrammar()` at compile-time
- Parser tables are built
- Parser code is injected into the module
- Function `parseMath(input: string): ParseNode` is created

### 3. Use the Generated Parser

```nim
let tree = parseMath("1 + 2 * 3")
echo tree
```

The parser name is derived from the grammar name: `"math"` → `parseMath()` (CamelCase).

## DSL Functions Used

### Structure
- **`sym(name)`** - Reference another rule
- **`seq(...)`** - Sequence of elements
- **`choice(...)`** - Alternatives (OR)

### Repetition
- **`rep(rule)`** - One or more
- **`rep0(rule)`** - Zero or more (not used in this example)
- **`opt(rule)`** - Optional (not used in this example)

### Lexical
- **`token(rule)`** - Mark as lexical token
- **`patt(regex)`** - Regular expression pattern

### Precedence
- **`prec_left(n, rule)`** - Left associativity with precedence level n

## Understanding Precedence

The example uses `prec_left(1, ...)` for binary operations. This ensures:

1. **Left Associativity**: `1 + 2 + 3` is parsed as `(1 + 2) + 3`
2. **Proper Grouping**: Operations are grouped correctly

For multiple precedence levels (e.g., `*` higher than `+`), you would use different precedence numbers:

```nim
# Higher precedence for multiplication
prec_left(2, seq(sym("expr"), str("*"), sym("expr")))

# Lower precedence for addition
prec_left(1, seq(sym("expr"), str("+"), sym("expr")))
```

## Advantages Over Other Approaches

### vs. `importGrammar`

| Feature | `buildGrammar` | `importGrammar` |
|---------|---------------|-----------------|
| JavaScript dependencies | ❌ None | ✅ Node.js/Bun required |
| Type safety | ✅ Full Nim type checking | ⚠️ Limited |
| IDE support | ✅ Autocomplete, errors | ⚠️ External file |
| Refactoring | ✅ Easy | ⚠️ Manual |
| External scanners | ❌ Not supported | ✅ Supported |
| Existing grammars | ❌ Can't reuse | ✅ Can reuse |

### vs. Manual Generation

| Feature | `buildGrammar` | Manual |
|---------|----------------|--------|
| File management | ✅ Single file | ⚠️ Multiple files |
| Workflow | ✅ One step | ⚠️ Multi-step |
| Type safety | ✅ Type-checked DSL | ⚠️ String-based |
| Intermediate files | ✅ None | ⚠️ parser.nim |

## Customizing for Your Grammar

To use this pattern for your own language:

```nim
import treestand
import std/options

proc createMyGrammar(): InputGrammar =
  InputGrammar(
    name: "my_lang",  # This determines the function name: parseMyLang()
    variables: @[
      # Define your rules here
      Variable(name: "program", kind: vtNamed, rule: ...),
      # ...
    ],
    extraSymbols: @[token(patt("\\s+"))]  # Skip whitespace
  )

buildGrammar(createMyGrammar)

when isMainModule:
  let tree = parseMyLang("your input here")
  echo tree
```

## Variable Kinds

The `kind` field in `Variable` specifies how the rule appears in the parse tree:

- **`vtNamed`** - Normal rule, appears in the tree (most common)
- **`vtAnonymous`** - Hidden name, but rule content appears
- **`vtHidden`** - Completely hidden from the tree
- **`vtAuxiliary`** - Helper rule

Most rules should use `vtNamed`.

## Generated API

After calling `buildGrammar`, you get:

### Main Parser Function
```nim
proc parseMath(input: string): ParseNode
```
- Name derived from grammar name (`"math"` → `parseMath`)
- Returns the root of the parse tree

### Parser Constructor
```nim
proc newParser(input: string): Parser
```
- Same for all grammars (not grammar-specific)
- For advanced usage

### Parse Tree Types
```nim
type ParseNode* = ref object
  symbol*: Symbol
  children*: seq[ParseNode]
  token*: Token
  startPos*, endPos*: int
  startPoint*, endPoint*: Point
```

## Requirements

- **Nim**: The only requirement (no JavaScript needed!)
- **Treestand**: `nimble install treestand`

## Compilation Time

Grammar processing happens at compile-time, so:
- First compilation may take a few seconds
- Subsequent compilations are faster (Nim's incremental compilation)
- Runtime performance is unaffected

## See Also

- [docs/build_grammar.md](../../docs/build_grammar.md) - Complete DSL reference and advanced patterns
- [docs/import_grammar.md](../../docs/import_grammar.md) - Importing Tree-sitter grammars
- [docs/using_dsl.md](../../docs/using_dsl.md) - Detailed DSL function documentation
- [Example 05](../05_importGrammar) - Using `importGrammar` with grammar.js files
