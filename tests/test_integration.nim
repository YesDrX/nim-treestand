import unittest, options
import treestand

suite "Integration - buildTables":
  
  test "buildTables - complete pipeline":
    # Create a simple grammar:  S -> 'a'
    # With lexical token 'a'
    
    # Build input grammar 
    let variables = @[Variable(
      name: "start",
      kind: vtNamed,
      rule: Rule(kind: rkString, stringValue: "a")
    )]
    
    let inputGrammar = InputGrammar(
      name: "test",
      variables: variables,
      extraSymbols: @[],
      expectedConflicts: @[],
      precedenceOrderings: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(string),
      reservedWords: @[]
    )
    
    # Prepare grammar
    let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
    
    # Build tables
    let tables = buildTables(syntaxGrammar, lexicalGrammar)
    
    # Verify we got tables
    check tables.mainLexTable.states.len > 0
    check tables.parseTable.entries.len > 0
    check tables.parseTable.productionInfos.len > 0
