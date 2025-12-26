import unittest, options, tables, sets
import treestand
import treestand/symbol_set

suite "LR Parse Table - FIRST Sets":
  
  test "computeFirst - single terminal":
    # Grammar: A -> 'a'
    let prodA = Production(
      steps: @[ProductionStep(
        symbol: GrammarSymbol(kind: stTerminal, index: 0),
        precedence: nonePrecedence(),
        associativity: none(GrammarAssociativity),
        alias: none(Alias),
        fieldName: none(string),
        reservedWordSetId: NoReservedWords
      )],
      dynamicPrecedence: 0
    )
    
    let varA = SyntaxVariable(
      name: "A",
      kind: vtNamed,
      productions: @[prodA]
    )
    
    let grammar = SyntaxGrammar(
      variables: @[varA],
      extraSymbols: @[],
      expectedConflicts: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(GrammarSymbol),
      precedenceOrderings: @[],
      reservedWordSets: @[]
    )


    let firstSets = computeFirst(grammar)
    
    # FIRST(A) should contain terminal symbol with index 0
    let symA = GrammarSymbol(kind: stNonTerminal, index: 0)
    let termA = GrammarSymbol(kind: stTerminal, index: 0)
    
    # Check using getOrDefault
    let firstA = firstSets.getOrDefault(symA)
    check firstA.len == 1
    check firstA.contains(termA)

  test "computeFollow - simple":
    # Grammar: S -> A 'b', A -> 'a'
    # FOLLOW(A) should contain 'b'
    let prodA = Production(
      steps: @[ProductionStep(
        symbol: GrammarSymbol(kind: stTerminal, index: 0),  # 'a'
        precedence: nonePrecedence(),
        associativity: none(GrammarAssociativity),
        alias: none(Alias),
        fieldName: none(string),
        reservedWordSetId: NoReservedWords
      )],
      dynamicPrecedence: 0
    )
    
    let prodS = Production(
      steps: @[
        ProductionStep(
          symbol: GrammarSymbol(kind: stNonTerminal, index: 1),  # A
          precedence: nonePrecedence(),
          associativity: none(GrammarAssociativity),
          alias: none(Alias),
          fieldName: none(string),
          reservedWordSetId: NoReservedWords
        ),
        ProductionStep(
          symbol: GrammarSymbol(kind: stTerminal, index: 1),  # 'b'
          precedence: nonePrecedence(),
          associativity: none(GrammarAssociativity),
          alias: none(Alias),
          fieldName: none(string),
          reservedWordSetId: NoReservedWords
        )
      ],
      dynamicPrecedence: 0
    )
    
    let varS = SyntaxVariable(name: "S", kind: vtNamed, productions: @[prodS])
    let varA = SyntaxVariable(name: "A", kind: vtNamed, productions: @[prodA])
    
    let grammar = SyntaxGrammar(
      variables: @[varS, varA],
      extraSymbols: @[],
      expectedConflicts: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(GrammarSymbol),
      precedenceOrderings: @[],
      reservedWordSets: @[]
    )
    
    let firstSets = computeFirst(grammar)
    let followSets = computeFollow(grammar, firstSets)
    
    let symA = GrammarSymbol(kind: stNonTerminal, index: 1)
    let termB = GrammarSymbol(kind: stTerminal, index: 1)
    
    # FOLLOW(A) should contain 'b'
    let followA = followSets.getOrDefault(symA)
    check followA.len == 1
    check followA.contains(termB)
  
  test "computeFollow - end marker":
    # Grammar: S -> A, A -> 'a'
    # FOLLOW(A) should contain $ (end marker)
    let prodA = Production(
      steps: @[ProductionStep(
        symbol: GrammarSymbol(kind: stTerminal, index: 0),
        precedence: nonePrecedence(),
        associativity: none(GrammarAssociativity),
        alias: none(Alias),
        fieldName: none(string),
        reservedWordSetId: NoReservedWords
      )],
      dynamicPrecedence: 0
    )
    
    let prodS = Production(
      steps: @[ProductionStep(
        symbol: GrammarSymbol(kind: stNonTerminal, index: 1),  # A
        precedence: nonePrecedence(),
        associativity: none(GrammarAssociativity),
        alias: none(Alias),
        fieldName: none(string),
        reservedWordSetId: NoReservedWords
      )],
      dynamicPrecedence: 0
    )
    
    let varS = SyntaxVariable(name: "S", kind: vtNamed, productions: @[prodS])
    let varA = SyntaxVariable(name: "A", kind: vtNamed, productions: @[prodA])
    
    let grammar = SyntaxGrammar(
      variables: @[varS, varA],
      extraSymbols: @[],
      expectedConflicts: @[],
      externalTokens: @[],
      variablesToInline: @[],
      supertypeSymbols: @[],
      wordToken: none(GrammarSymbol),
      precedenceOrderings: @[],
      reservedWordSets: @[]
    )
    
    let firstSets = computeFirst(grammar)
    let followSets = computeFollow(grammar, firstSets)
    
    let symA = GrammarSymbol(kind: stNonTerminal, index: 1)
    let endSym = GrammarSymbol(kind: stEnd, index: 0)
    
    # FOLLOW(A) should contain $
    let followA = followSets.getOrDefault(symA)
    check followA.len == 1
    check followA.contains(endSym)
