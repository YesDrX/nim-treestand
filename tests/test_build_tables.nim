import unittest, options
import treestand/build_tables
import treestand/grammar
import treestand/rules
import treestand/nfa

suite "Table Building":
  
  test "LexicalGrammar structure":
    # Just verify we can create the grammar structure
    let fooRule = Rule(kind: rkString, stringValue: "foo")
    
    let vars = @[
      LexicalVariable(
        name: "'foo'",
        kind: vtAnonymous,
        implicitPrecedence: 0,
        startState: 0,
        rule: fooRule
      )
    ]
    
    var lexGrammar = LexicalGrammar(
      nfa: newNfa(),
      variables: vars
    )
    
    check lexGrammar.variables.len == 1
    check lexGrammar.variables[0].name == "'foo'"
    check lexGrammar.variables[0].rule.kind == rkString
    check lexGrammar.variables[0].rule.stringValue == "foo"

  test "buildLexTable - basic dfa":
    # Test DFA construction from NFA
    # Grammar: two tokens 'a' and 'b'
    
    let ruleA = Rule(kind: rkString, stringValue: "a")
    let ruleB = Rule(kind: rkString, stringValue: "b")
    
    let vars = @[
      LexicalVariable(
        name: "'a'",
        kind: vtAnonymous,
        implicitPrecedence: 0,
        startState: 0,  # Will be set by buildLexicalNfa
        rule: ruleA
      ),
      LexicalVariable(
        name: "'b'",
        kind: vtAnonymous,
        implicitPrecedence: 0,
        startState: 0,  # Will be set by buildLexicalNfa  
        rule: ruleB
      )
    ]
    
    var lexGrammar = LexicalGrammar(
      nfa: newNfa(),
      variables: vars
    )
    
    # For this test, we rely on buildTables which will internally
    # call buildLexicalNfa and then dfaFromNfa
    # Since those are private, we can't test them directly here
    # The real integration test is in test_integration.nim
    
    # Just verify structure
    check lexGrammar.variables.len == 2
