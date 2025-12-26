## Test suite for complex real-world grammars
## This verifies that grammars with complex precedence and quantifiers can be generated

import unittest, os
import treestand/[parse_grammar, prepare_grammar, build_tables]

const testDir = currentSourcePath.parentDir

suite "Complex Grammar Generation Tests":
  test "Generate Meson grammar":
    ## Meson grammar uses named precedence levels and multiple expression types
    ## that can all shift the same symbols (e.g., '[' for subscript)
    let grammarPath = testDir / "fixtures/meson/grammar.js"
    
    check fileExists(grammarPath)
    
    # Parse and prepare the grammar
    let inputGrammar = parseGrammarJs(grammarPath)
    check inputGrammar.name == "meson"
    
    # Prepare grammar (should not raise)
    let (syntax, lexical) = prepareGrammar(inputGrammar)
    check syntax.variables.len > 0
    
    # Build parse tables (should not raise conflicts)
    let tables = buildTables(syntax, lexical)
    check tables.parseTable.entries.len > 0
    
    echo "✓ Meson grammar generated successfully with ", tables.parseTable.entries.len, " states"
  
  test "Generate Go grammar":
    ## Go grammar includes regex patterns with {n,m} quantifiers in escape sequences
    ## like /u[0-9a-fA-F]{4}/ which are handled by tree-sitter's JS layer
    let grammarPath = testDir / "fixtures/go/grammar.js"
    
    check fileExists(grammarPath)
    
    # Parse and prepare the grammar
    let inputGrammar = parseGrammarJs(grammarPath)
    check inputGrammar.name == "go"
    
    # Prepare grammar (should not raise)
    let (syntax, lexical) = prepareGrammar(inputGrammar)
    check syntax.variables.len > 0
    
    # Build parse tables (should not raise)
    let tables = buildTables(syntax, lexical)
    check tables.parseTable.entries.len > 0
    
    echo "✓ Go grammar generated successfully with ", tables.parseTable.entries.len, " states"
