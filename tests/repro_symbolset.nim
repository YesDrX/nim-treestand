
import std/tables
import treestand/symbol_set
import treestand/rules
import std/intsets
import std/unittest

# Mock GrammarSymbol creation
proc sym(k: GrammarSymbolType, i: int): GrammarSymbol =
  GrammarSymbol(kind: k, index: i.uint16)

test "SymbolSet Persistence in Table":
  var t = initTable[GrammarSymbol, SymbolSet]()
  let key = sym(stNonTerminal, 1)
  t[key] = initSymbolSet()
  
  # accessing via [] matches `computeFirst`
  t[key].incl(sym(stTerminal, 2))
  
  echo "Count after incl: ", t[key].len
  check t[key].len == 1
  check sym(stTerminal, 2) in t[key]
  
  # accessing via var ptr (just to see)
  t.withValue(key, val):
    val[].incl(sym(stTerminal, 3))
    
  echo "Count after withValue: ", t[key].len
  check t[key].len == 2
  
test "SymbolSet Copy Semantics":
  var s1 = initSymbolSet()
  s1.incl(sym(stTerminal, 1))
  
  var s2 = s1
  s2.incl(sym(stTerminal, 2))
  
  # IntSet should be copy-on-write or distinct? 
  # If s2 is a copy, s1 should have 1 item
  echo "s1 len: ", s1.len
  echo "s2 len: ", s2.len
  
