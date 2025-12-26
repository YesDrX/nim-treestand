import treestand
import std/os

# ==============================================================================
# Grammar Definition
# ==============================================================================

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
  generateParser(g, currentSourcePath().parentDir() / "parser.nim")
  echo "Parser generated to ", currentSourcePath().parentDir() / "parser.nim"
