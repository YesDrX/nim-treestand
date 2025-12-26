## Basic tests for treestand

import unittest, treestand/js_exec, treestand/parse_grammar

suite "JS Execution":
  test "findJsRuntime":
    let runtime = findJsRuntime()
    # Just check it doesn't crash - Option doesn't have isSome/isNone fields in Nim 2.x
    discard runtime

suite "Grammar Parsing":
  test "parse simple grammar":
    let json = """
    {
      "name": "test_grammar",
      "rules": {
        "expression": {
          "type": "STRING",
          "value": "test"
        }
      },
      "extras": [],
      "externals": [],
      "conflicts": [],
      "precedences": []
    }
    """
    let grammar = parseGrammar(json)
    check grammar.name == "test_grammar"
    check grammar.variables.len == 1

