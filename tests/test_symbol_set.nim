## Test SymbolSet to ensure correctness and API compatibility

import unittest, std/sets
import ../src/treestand/symbol_set, ../src/treestand/rules

suite "SymbolSet Tests":
  test "Basic operations":
    var s = initSymbolSet()
    
    # Add symbols
    let sym1 = newSymbol(stTerminal, 5)
    let sym2 = newSymbol(stTerminal, 3)
    let sym3 = newSymbol(stNonTerminal, 1)
    
    s.incl(sym1)
    s.incl(sym2)
    s.incl(sym3)
    
    check s.len == 3
    check sym1 in s
    check sym2 in s
    check sym3 in s
    
    # Check non-existent
    let sym4 = newSymbol(stTerminal, 99)
    check sym4 notin s
  
  test "Duplicate handling":
    var s = initSymbolSet()
    
    let sym = newSymbol(stTerminal, 1)
    s.incl(sym)
    s.incl(sym)  # Add again
    
    check s.len == 1  # Should not duplicate
  
  test "Removal":
    var s = initSymbolSet()
    
    let sym1 = newSymbol(stTerminal, 1)
    let sym2 = newSymbol(stTerminal, 2)
    
    s.incl(sym1)
    s.incl(sym2)
    check s.len == 2
    
    s.excl(sym1)
    check s.len == 1
    check sym1 notin s
    check sym2 in s
  
  test "Iteration order":
    var s = initSymbolSet()
    
    # Add in random order
    s.incl(newSymbol(stTerminal, 5))
    s.incl(newSymbol(stTerminal, 1))
    s.incl(newSymbol(stTerminal, 3))
    s.incl(newSymbol(stNonTerminal, 0))
    
    # Check all symbols are present via iteration
    var count = 0
    for sym in s:
      count += 1
    check count == 4
  
  test "Conversion to/from HashSet":
    var s = initSymbolSet()
    s.incl(newSymbol(stTerminal, 1))
    s.incl(newSymbol(stTerminal, 2))
    s.incl(newSymbol(stNonTerminal, 3))
    
    let hs = s.toHashSet()
    check hs.len == 3
    
    let s2 = hs.toSymbolSet()
    check s2.len == 3
    check s == s2
  
  test "Mixed symbol kinds":
    var s = initSymbolSet()
    
    s.incl(newSymbol(stTerminal, 5))
    s.incl(newSymbol(stNonTerminal, 2))
    s.incl(newSymbol(stExternal, 1))
    s.incl(newSymbol(stEnd, 0))
    
    check s.len == 4
    check newSymbol(stTerminal, 5) in s
    check newSymbol(stNonTerminal, 2) in s
    check newSymbol(stExternal, 1) in s
    check newSymbol(stEnd, 0) in s
  
  test "Large set":
    var s = initSymbolSet()
    
    # Add many symbols
    for i in 0 ..< 100:
      s.incl(newSymbol(stTerminal, i))
    
    check s.len == 100
    
    # Check all present
    for i in 0 ..< 100:
      check newSymbol(stTerminal, i) in s
    
    # Check non-existent
    check newSymbol(stTerminal, 999) notin s
