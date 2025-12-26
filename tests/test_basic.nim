## Basic tests for treestand

import unittest
import treestand/[js_exec, parse_grammar]

suite "JS Execution":
  test "findJsRuntime":
    let runtime = findJsRuntime()
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

