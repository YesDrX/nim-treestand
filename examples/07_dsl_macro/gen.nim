import std/options
import treestand

# Define the grammar using the `ts_grammar` macro.
# This macro allows defining rules using a concise syntax similar to PEG/EBNF.
ts_grammar "calc":
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

import std/os

when isMainModule:
  echo "Generating Calc parser..."
  # The macro generates a proc named `calc` (same as grammar name)
  let g = calc()
  generateParser(g, currentSourcePath().parentDir() / "parser.nim")
  echo "Done!"
