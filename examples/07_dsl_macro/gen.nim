import std/[options, tables]
import treestand

# ==============================================================================
# Example 1: Basic Grammar Definition
# ==============================================================================

# Define the grammar using the `tsGrammar` macro.
# This macro allows defining rules using a concise syntax similar to PEG/EBNF.
tsGrammar "calc":
  # Main entry point
  program     <- +stmt
  
  # Choice using `|`
  stmt        <- (expression * semi) | empty_stmt
  
  # Sequence using `*`
  # Named fields using (name: rule)
  binary      <- ((left: expression) * op * (right: expression)) ^ 1
  
  # Standard rules
  expression  <- number | binary | parens
  parens      <- lparen * expression * rparen
  
  # Set syntax for keywords
  keyword     <- {"if", "else", "while"}
  
  # Tokens using `token()`
  number      <- token(re"\d+")
  op          <- token(re"[+\-*/]")
  semi        <- token(";")
  lparen      <- token("(")
  rparen      <- token(")")
  empty_stmt  <- token("")
  identifier  <- token(re"[a-zA-Z_][a-zA-Z0-9_]*")
  
  # Configuration properties
  # Use `field = value` syntax.
  extras      = token(re"\s+")
  word        = "identifier"

# ==============================================================================
# Example 2: Grammar with Embedded Actions
# ==============================================================================

type
  Stats = object
    exprCount: int
    maxDepth: int

tsGrammar "calcWithActions", userdata: Stats:
  program <- +stmt:
    echo "Parsed ", userdata.exprCount, " expressions"
  
  stmt <- expression * semi:
    userdata.exprCount += 1
  
  expression <- number | binary | parens
  
  binary <- expression * op * expression:
    echo "Found binary op: ", node.child(1).text
  
  parens <- lparen * expression * rparen:
    echo "Found parenthesized expression"
  
  number <- token(re"\d+")
  op <- token(re"[+\-*/]")
  semi <- token(";")
  lparen <- token("(")
  rparen <- token(")")
  
  extras = token(re"\s+")

import std/os

when isMainModule:
  echo "=== Example 1: Basic Grammar Generation ==="
  # The macro generates a proc named `calc` (same as grammar name)
  let g = calc()
  generateParser(g, currentSourcePath().parentDir() / "parser.nim")
  echo "Parser generated to parser.nim"
  echo ""
  
  echo "=== Example 2: Grammar with Actions ==="
  var stats = Stats()
  let input = "1 + 2; 3 * 4; (5 + 6) * 7;"
  echo "Input: ", input
  
  if matchCalcWithActions(input, stats):
    echo "Success! Parsed ", stats.exprCount, " statements"
  else:
    echo "Parse failed"
  
  echo "\n=== Complete ==="
