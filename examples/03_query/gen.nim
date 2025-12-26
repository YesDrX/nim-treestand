import treestand
import std/os

proc createMathGrammar(): InputGrammar =
  InputGrammar(
    name: "math",
    variables: @[
      Variable(name: "program", kind: vtNamed, rule: rep(sym("expression"))),
      Variable(name: "expression", kind: vtNamed, rule: choice(sym("number"), sym("binary_op"))),
      Variable(name: "binary_op", kind: vtNamed, rule: prec_left(1, seq(sym("expression"), sym("op"), sym("expression")))),
      Variable(name: "number", kind: vtNamed, rule: token(patt("\\d+"))),
      Variable(name: "op", kind: vtNamed, rule: token(patt("[+\\-*/]")))
    ],
    extraSymbols: @[ token(patt("\\s+")) ]
  )

when isMainModule:
  let g = createMathGrammar()
  generateParser(g, currentSourcePath().parentDir() / "parser.nim")
  echo "Parser generated to ", currentSourcePath().parentDir() / "parser.nim"