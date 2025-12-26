import unittest, options, tables
import treestand/prepare_grammar
import treestand/grammar
import treestand/rules
import treestand/nfa

suite "Grammar Preparation":
  
  test "extractTokens - basic string extraction":
    # Create a simple grammar with a string rule
    let rule = Rule(kind: rkString, stringValue: "keyword")
    let variable = Variable(name: "test_rule", kind: vtNamed, rule: rule)
    let input = InputGrammar(
      name: "test",
      variables: @[variable],
      extraSymbols: @[],
      expectedConflicts: @[],
      precedenceOrderings: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(string),
      reservedWords: @[]
    )
    
    let (syntax, lexical) = prepareGrammar(input)
    
    # Validation
    # 1. The syntax rule should now refer to a terminal symbol
    check syntax.variables.len == 1
    # We expect one production step pointing to a terminal
    check syntax.variables[0].productions.len == 1
    check syntax.variables[0].productions[0].steps.len == 1
    check syntax.variables[0].productions[0].steps[0].symbol.kind == stTerminal
    
    # 2. The lexical grammar should contain the "keyword"
    # Note: Precise NFA structure checking is hard, but we can check we have a variable
    check lexical.variables.len >= 1

  test "expandRepeats - basic repeat expansion":
    # Rule: A = repeat(B)
    # Should become: A = A_repeat1, A_repeat1 = B A_repeat1 | epsilon (or similar left/right recursion)
    
    let bSymbol = Rule(kind: rkNamedSymbol, symbolName: "B")
    var symbolRef = new(Rule)
    symbolRef[] = bSymbol
    let repeatRule = Rule(kind: rkRepeat, repeatContent: symbolRef)
    let aVar = Variable(name: "A", kind: vtNamed, rule: repeatRule)
    let bVar = Variable(name: "B", kind: vtNamed, rule: Rule(kind: rkString, stringValue: "b"))
    
    let input = InputGrammar(
      name: "test_repeat",
      variables: @[aVar, bVar], # A is start rule
      extraSymbols: @[],
      expectedConflicts: @[],
      precedenceOrderings: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(string),
      reservedWords: @[]
    )
    
    let (syntax, lexical) = prepareGrammar(input)
    
    # Check that A is preserved but its content is changed to point to auxiliary
    let aSyntax = syntax.variables[0]
    check aSyntax.name == "A"
    
    # There should be an auxiliary variable created for the repeat
    check syntax.variables.len > 2 # A, B, and A_repeat...
    
  test "processInlines - basic inlining":
    # A = B, B = "b", inline B
    let bRule = Rule(kind: rkString, stringValue: "b")
    let aRule = Rule(kind: rkNamedSymbol, symbolName: "B")
    
    let aVar = Variable(name: "A", kind: vtNamed, rule: aRule)
    let bVar = Variable(name: "B", kind: vtNamed, rule: bRule)
    
    let input = InputGrammar(
      name: "test_inline",
      variables: @[aVar, bVar],
      variablesToInline: @["B"],
      extraSymbols: @[],
      expectedConflicts: @[],
      precedenceOrderings: @[],
      externalTokens: @[],
      supertypeSymbols: @[],
      wordToken: none(string),
      reservedWords: @[]
    )
    
    let (syntax, lexical) = prepareGrammar(input)
    
    # B should be gone from the main list or marked as inlined/removed, 
    # but more importantly A should directly contain the content of B (the string "b")
    # which will then be extracted as a token.
    
    let aSyntax = syntax.variables[0]
    check aSyntax.name == "A"
    # A should produce "b" (terminal) directly, not via non-terminal B
    check aSyntax.productions[0].steps[0].symbol.kind == stTerminal
