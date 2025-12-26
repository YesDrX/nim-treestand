import treestand
import std/[unittest, options]

# Define grammar at top level
ts_grammar "my_grammar":
  program     <- +expr
  expr        <- number | binary
  binary      <- ((left: expr) * op * (right: expr)) ^ 1
  keyword     <- {"if", "else"}
  number      <- token(re"\d+")
  op          <- token(re"[+\-*/]")
  indent      <- external_token
  
  extras      = token(re"\s+")
  conflicts   = @[ @["a", "b"] ]
  inline      = @["op"]
  word        = "identifier"

suite "DSL Macros":
  test "Grammar Generation":
    let g = my_grammar()
    check g.name == "my_grammar"
    check g.variables.len == 6 # indent is not in variables
    check g.extraSymbols.len == 1
    
    # Check structure of 'program'
    let program = g.variables[0]
    check program.name == "program"
    # +expr should be rep(sym("expr"))
    check program.rule.kind == rkRepeat
    check program.rule.repeatContent.kind == rkNamedSymbol
    check program.rule.repeatContent.symbolName == "expr"

    # Check precedence of binary
    let binRule = g.variables[2].rule
    # Precedence wrapper is likely top level
    check binRule.kind == rkMetadata
    check binRule.metadataParams.precedence.intValue == 1
    
    # Check named fields in binary
    # binary rule structure: metadata(prec, seq(seq(field(left, expr), sym(op)), field(right, expr)))
    # Because '*' is left-associative binary operator in macro transformation.
    let binSeq = binRule.metadataRule[]
    check binSeq.kind == rkSeq
    check binSeq.seqMembers.len == 2
    
    # Right operand
    let rightField = binSeq.seqMembers[1]
    check rightField.kind == rkMetadata
    check rightField.metadataParams.fieldName.isSome
    check rightField.metadataParams.fieldName.get == "right"
    
    # Left part (nested seq)
    let leftSeq = binSeq.seqMembers[0]
    check leftSeq.kind == rkSeq
    check leftSeq.seqMembers.len == 2
    
    let leftField = leftSeq.seqMembers[0]
    check leftField.kind == rkMetadata
    check leftField.metadataParams.fieldName.isSome
    check leftField.metadataParams.fieldName.get == "left"
    
    let opSym = leftSeq.seqMembers[1]
    check opSym.kind == rkNamedSymbol
    check opSym.symbolName == "op"
    
    # Check keyword set syntax
    # keyword <- {"if", "else"} -> choice(str("if"), str("else"))
    # find keyword variable
    var keywordVar: Variable
    for v in g.variables:
      if v.name == "keyword":
        keywordVar = v
        break
    
    check keywordVar.name == "keyword"
    check keywordVar.rule.kind == rkChoice
    check keywordVar.rule.choiceMembers.len == 2
    check keywordVar.rule.choiceMembers[0].kind == rkString
    check keywordVar.rule.choiceMembers[0].stringValue == "if"
    check keywordVar.rule.choiceMembers[1].kind == rkString
    check keywordVar.rule.choiceMembers[1].stringValue == "else"

    # Check conflicts
    check g.expectedConflicts.len == 1
    check g.expectedConflicts[0] == @["a", "b"]
    
    # Check external tokens
    check g.externalTokens.len == 1
    check g.externalTokens[0].kind == rkNamedSymbol
    check g.externalTokens[0].symbolName == "indent"

    # Check inline
    check g.variablesToInline.len == 1
    check g.variablesToInline[0] == "op"

    # Check word
    check g.wordToken.isSome
    check g.wordToken.get == "identifier"
