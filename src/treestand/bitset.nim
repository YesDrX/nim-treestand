## Efficient BitSet implementation for lookahead sets in LALR(1) parser generation.
##
## The words are stored INLINE (fixed-size array), so copying/unioning sets in
## hot loops never touches the heap. 1024 bits covers the combined terminal +
## external token count of every realistic grammar.

import grammar, hashes
import std/bitops

const BitsetWords* = 16  ## 1024 bits of inline storage

type
  BitSet* = object
    words*: array[BitsetWords, uint64]
    capacity*: int16  ## Number of bits this set "knows about" (grows on demand)

proc wordCount(capacity: int): int {.inline.} =
  (capacity + 63) div 64

proc newBitSet*(capacity: int): BitSet =
  ## Create a new empty BitSet that can hold `capacity` bits.
  if capacity > BitsetWords * 64:
    raise newException(ValueError, "BitSet capacity exceeded: " & $capacity &
      " bits requested, max is " & $(BitsetWords * 64))
  result = BitSet(capacity: capacity.int16)  # words are zero-initialized

proc initBitSet*(capacity: int = 128): BitSet =
  ## Initialize an empty BitSet with default capacity.
  newBitSet(capacity)

proc len*(bs: BitSet): int =
  ## Return the number of set bits (cardinality).
  result = 0
  for word in bs.words:
    result += countSetBits(word)

proc incl*(bs: var BitSet, bitIndex: int) =
  ## Set the bit at `bitIndex` to 1.
  if bitIndex >= BitsetWords * 64:
    raise newException(ValueError, "BitSet index out of range: " & $bitIndex)
  if bitIndex >= bs.capacity:
    bs.capacity = (bitIndex + 1).int16

  let wordIdx = bitIndex shr 6  # bitIndex div 64
  let bitIdx = bitIndex and 63   # bitIndex mod 64
  bs.words[wordIdx] = bs.words[wordIdx] or (1'u64 shl bitIdx)

proc excl*(bs: var BitSet, bitIndex: int) =
  ## Clear the bit at `bitIndex` (set to 0).
  if bitIndex >= bs.capacity or bitIndex < 0:
    return  # Nothing to clear

  let wordIdx = bitIndex shr 6
  let bitIdx = bitIndex and 63
  bs.words[wordIdx] = bs.words[wordIdx] and not (1'u64 shl bitIdx)

proc incl2*(bs: var BitSet, bitIndex: int): bool =
  ## Set the bit at `bitIndex` to 1. Returns true (for `discard`-free use
  ## inside `mgetOrPut(...)` call sites).
  bs.incl(bitIndex)
  true

proc contains*(bs: BitSet, bitIndex: int): bool =
  ## Check if bit at `bitIndex` is set.
  if bitIndex >= bs.capacity or bitIndex < 0:
    return false

  let wordIdx = bitIndex shr 6
  let bitIdx = bitIndex and 63
  (bs.words[wordIdx] and (1'u64 shl bitIdx)) != 0

proc union*(dest: var BitSet, src: BitSet): bool =
  ## Union `src` into `dest` using bitwise OR. Returns true if `dest` was modified.
  var changed = false

  # Perform word-level OR (only over the words `src` actually uses)
  let srcWords = wordCount(src.capacity.int)
  for i in 0 ..< srcWords:
    let oldWord = dest.words[i]
    dest.words[i] = oldWord or src.words[i]
    if dest.words[i] != oldWord:
      changed = true

  if src.capacity > dest.capacity:
    dest.capacity = src.capacity

  result = changed

proc intersect*(dest: var BitSet, src: BitSet) =
  ## Intersect `dest` with `src` using bitwise AND.
  # Words beyond `src`'s capacity are zero in `src`, so a full-width AND
  # also clears any `dest` bits outside of `src`'s range.
  for i in 0 ..< BitsetWords:
    dest.words[i] = dest.words[i] and src.words[i]

proc clear*(bs: var BitSet) =
  ## Clear all bits (set all to 0).
  for i in 0 ..< BitsetWords:
    bs.words[i] = 0

proc `==`*(a, b: BitSet): bool =
  ## Check if two BitSets are equal.
  a.words == b.words

proc hash*(bs: BitSet): Hash =
  ## Hash a BitSet for use in tables.
  result = Hash(0)
  for word in bs.words:
    if word != 0:  # Only hash non-zero words for efficiency
      result = result !& hash(word)
  result = !$result

iterator items*(bs: BitSet): int =
  ## Iterate over all set bit indices.
  for wordIdx in 0 ..< BitsetWords:
    let word = bs.words[wordIdx]
    if word != 0:
      for bitIdx in 0 ..< 64:
        if (word and (1'u64 shl bitIdx)) != 0:
          yield (wordIdx shl 6) + bitIdx

proc `$`*(bs: BitSet): string =
  ## String representation for debugging.
  result = "BitSet{"
  var first = true
  for bit in bs:
    if not first:
      result.add(", ")
    result.add($bit)
    first = false
  result.add("}")

# === Symbol Mapping Utilities ===

proc symbolToInt*(sym: GrammarSymbol): int =
  ## Convert a GrammarSymbol to a dense integer for BitSet indexing.
  ## Mapping: Terminals -> 0..N-1, Externals -> N..N+M-1, End -> N+M
  case sym.kind
  of stTerminal:
    sym.index.int
  of stExternal:
    # Externals come after terminals
    # We need to know the terminal count - this will be set by caller
    # For now, use a large offset (will be adjusted in actual usage)
    10000 + sym.index.int
  of stEnd:
    20000  # Well beyond any expected terminal/external count
  of stEndOfNonTerminalExtra:
    20001
  else:
    # Non-terminals shouldn't be in lookahead sets
    -1

proc intToSymbol*(value: int, terminalCount: int, externalCount: int): GrammarSymbol =
  ## Convert a dense integer back to a GrammarSymbol.
  ## Inverse of symbolToInt.
  if value < terminalCount:
    GrammarSymbol(kind: stTerminal, index: value.uint16)
  elif value < terminalCount + externalCount:
    GrammarSymbol(kind: stExternal, index: (value - terminalCount).uint16)
  elif value == terminalCount + externalCount:
    GrammarSymbol(kind: stEnd, index: 0)
  elif value == terminalCount + externalCount + 1:
    GrammarSymbol(kind: stEndOfNonTerminalExtra, index: 0)
  else:
    # Invalid mapping
    GrammarSymbol(kind: stEnd, index: 0)

# === Symbol Context for proper mapping ===

type
  SymbolContext* = object
    ## Context for mapping symbols to/from BitSet indices.
    terminalCount*: int
    externalCount*: int
    maxIndex*: int  # terminalCount + externalCount + 2 (for End symbols)

proc newSymbolContext*(terminalCount, externalCount: int): SymbolContext =
  ## Create a symbol context for proper mapping.
  SymbolContext(
    terminalCount: terminalCount,
    externalCount: externalCount,
    maxIndex: terminalCount + externalCount + 2
  )

proc symbolToBit*(ctx: SymbolContext, sym: GrammarSymbol): int =
  ## Convert symbol to bit index using context.
  case sym.kind
  of stTerminal:
    sym.index.int
  of stExternal:
    ctx.terminalCount + sym.index.int
  of stEnd:
    ctx.terminalCount + ctx.externalCount
  of stEndOfNonTerminalExtra:
    ctx.terminalCount + ctx.externalCount + 1
  else:
    -1  # Non-terminals not in lookahead

proc bitToSymbol*(ctx: SymbolContext, bit: int): GrammarSymbol =
  ## Convert bit index back to symbol using context.
  if bit < ctx.terminalCount:
    GrammarSymbol(kind: stTerminal, index: bit.uint16)
  elif bit < ctx.terminalCount + ctx.externalCount:
    GrammarSymbol(kind: stExternal, index: (bit - ctx.terminalCount).uint16)
  elif bit == ctx.terminalCount + ctx.externalCount:
    GrammarSymbol(kind: stEnd, index: 0)
  elif bit == ctx.terminalCount + ctx.externalCount + 1:
    GrammarSymbol(kind: stEndOfNonTerminalExtra, index: 0)
  else:
    GrammarSymbol(kind: stEnd, index: 0)  # Fallback
