## Fast symbol set using packed int32 representation with IntSet
## GrammarSymbol is packed into 32 bits: kind (int16) + index (uint16)
## This allows using Nim's ultra-fast IntSet instead of hashing or sorting

import grammar
import std/[intsets, sets]

type
  SymbolSet* = object
    ## IntSet-based symbol set using packed 32-bit representation
    ## Extremely fast: O(1) operations, bit-based, no hashing needed
    intSet: IntSet

proc pack(sym: GrammarSymbol): int32 {.inline.} =
  when nimvm:
    sym.kind.int32 shl 16 or sym.index.int32
  else:
    cast[int32](sym)

proc unpack(packed: int32): GrammarSymbol {.inline.} =
  when nimvm:
    GrammarSymbol(kind: GrammarSymbolType(packed shr 16), index: (packed and 0xFFFF).uint16)
  else:
    cast[GrammarSymbol](packed)

proc initSymbolSet*(capacity: int = 8): SymbolSet =
  ## Create a new empty symbol set
  ## Note: capacity hint is ignored for IntSet (automatically managed)
  result.intSet = initIntSet()

proc len*(s: SymbolSet): int =
  ## Number of symbols in the set
  s.intSet.len

proc contains*(s: SymbolSet, sym: GrammarSymbol): bool {.inline.} =
  ## Check if symbol is in the set - O(1)
  pack(sym) in s.intSet

proc incl*(s: var SymbolSet, sym: GrammarSymbol) {.inline.} =
  ## Add symbol to the set if not already present - O(1)
  s.intSet.incl(pack(sym))

proc excl*(s: var SymbolSet, sym: GrammarSymbol) {.inline.} =
  ## Remove symbol from the set if present - O(1)
  s.intSet.excl(pack(sym))

iterator items*(s: SymbolSet): GrammarSymbol =
  ## Iterate over all symbols in the set
  for packed in s.intSet:
    yield unpack(int32(packed))

proc `==`*(a, b: SymbolSet): bool =
  ## Check if two sets are equal
  a.intSet == b.intSet

proc `$`*(s: SymbolSet): string =
  ## String representation for debugging
  result = "SymbolSet{"
  var first = true
  for sym in s:
    if not first:
      result.add(", ")
    result.add($sym)
    first = false
  result.add("}")

# Conversion helpers for compatibility

proc toHashSet*(s: SymbolSet): HashSet[GrammarSymbol] =
  ## Convert to HashSet for compatibility with existing code
  result = initHashSet[GrammarSymbol](s.len)
  for sym in s:
    result.incl(sym)

proc toSymbolSet*(hs: HashSet[GrammarSymbol]): SymbolSet =
  ## Convert from HashSet
  result = initSymbolSet()
  for sym in hs:
    result.incl(sym)
