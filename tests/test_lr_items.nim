import unittest, options, tables, sets
import treestand
import treestand/build_tables

suite "LR(1) Items":
  
  test "closure - simple":
    # Grammar: S -> A, A -> 'a'
    # Item: [S -> •A, $]
    # Closure should add: [A -> •'a', $]
    
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
    
    # Initial item: [S -> •A, $]
    let initialItem = LR1Item(
      variableIndex: 0,  # S
      productionIndex: 0,
      position: 0,  # Before A
      lookahead: GrammarSymbol(kind: stEnd, index: 0)
    )
    
    var items = initHashSet[LR1Item]()
    items.incl(initialItem)
    
    let closureItems = closure(grammar, items, firstSets)
    
    # Should contain original item
    check initialItem in closureItems
    
    # Should contain [A -> •'a', $]
    let expectedItem = LR1Item(
      variableIndex: 1,  # A
      productionIndex: 0,
      position: 0,  # Before 'a'
      lookahead: GrammarSymbol(kind: stEnd, index: 0)
    )
    
    check expectedItem in closureItems
    check closureItems.len == 2
  
  test "goto - advance dot":
    # Grammar: S -> 'a' 'b'
    # Item: [S -> 'a' • 'b', $]
    # GOTO on 'b' should give: [S -> 'a' 'b' •, $]
    
    let prodS = Production(
      steps: @[
        ProductionStep(
          symbol: GrammarSymbol(kind: stTerminal, index: 0),  # 'a'
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
    
    let grammar = SyntaxGrammar(
      variables: @[varS],
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
    
    # Item: [S -> 'a' • 'b', $]
    let item = LR1Item(
      variableIndex: 0,
      productionIndex: 0,
      position: 1,  # After 'a', before 'b'
      lookahead: GrammarSymbol(kind: stEnd, index: 0)
    )
    
    var items = initHashSet[LR1Item]()
    items.incl(item)
    
    let termB = GrammarSymbol(kind: stTerminal, index: 1)
    let gotoItems = goto(grammar, items, termB, firstSets)
    
    # Should contain [S -> 'a' 'b' •, $]
    let expectedItem = LR1Item(
      variableIndex: 0,
      productionIndex: 0,
      position: 2,  # After 'b'
      lookahead: GrammarSymbol(kind: stEnd, index: 0)
    )
    
    check expectedItem in gotoItems
    check gotoItems.len == 1
  
  test "buildCanonicalCollection - simple grammar":
    # Grammar: S -> 'a'
    # Should have at least initial state and state after seeing 'a'
    
    let prodS = Production(
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
    
    let varS = SyntaxVariable(name: "S", kind: vtNamed, productions: @[prodS])
    
    let grammar = SyntaxGrammar(
      variables: @[varS],
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
    let (states, stateMap, transitions) = buildCanonicalCollection(grammar, firstSets)
    
    # Should have at least 2 states
    # State 0: [S -> •'a', $]
    # State 1: [S -> 'a'•, $]
    check states.len >= 2
    check stateMap.len >= 2
    # Verify transitions
    # State 0 (Start) should transition on 'a' to State 1
    let termA = GrammarSymbol(kind: stTerminal, index: 0)
    
    # Check if there is a transition on 'a' from state 0
    # Note: State 0 is always the start state in our implementation
    check transitions[0].hasKey(termA)
    let targetStateId = transitions[0][termA]
    
    # Check that the target state contains [S -> 'a'•, $]
    let targetItem = LR1Item(
      variableIndex: 0,
      productionIndex: 0,
      position: 1,  # After 'a'
      lookahead: GrammarSymbol(kind: stEnd, index: 0)  # Inherited from initial item
    )
    
    # Find the set for the target state (we can iterate states or assume index aligns if we just created them)
    # The states seq is indexed by state ID.
    check targetStateId < states.len
    check targetItem in states[targetStateId]
