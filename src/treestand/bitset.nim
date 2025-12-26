## Efficient BitSet implementation for lookahead sets in LALR(1) parser generation.
## Provides O(1) set operations using bitwise operations on uint64 words.

import rules, hashes
import std/bitops

type
  BitSet* = object
    ## Compact bitset for representing sets of terminal symbols.
    ## Uses seq[uint64] to support arbitrary number of terminals.
    words*: seq[uint64]
    capacity*: int  # Total number of bits (symbols) this set can represent

proc newBitSet*(capacity: int): BitSet =
  ## Create a new empty BitSet that can hold `capacity` bits.
  let wordCount = (capacity + 63) div 64  # Round up to nearest word
  result = BitSet(
    words: newSeq[uint64](wordCount),
    capacity: capacity
  )

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
  if bitIndex >= bs.capacity:
    # Expand capacity if needed
    let newCapacity = max(bitIndex + 1, bs.capacity * 2)
    let newWordCount = (newCapacity + 63) div 64
    bs.words.setLen(newWordCount)
    bs.capacity = newCapacity
  
  let wordIdx = bitIndex shr 6  # bitIndex div 64
  let bitIdx = bitIndex and 63   # bitIndex mod 64
  bs.words[wordIdx] = bs.words[wordIdx] or (1'u64 shl bitIdx)

proc excl*(bs: var BitSet, bitIndex: int) =
  ## Clear the bit at `bitIndex` (set to 0).
  if bitIndex >= bs.capacity:
    return  # Nothing to clear
  
  let wordIdx = bitIndex shr 6
  let bitIdx = bitIndex and 63
  bs.words[wordIdx] = bs.words[wordIdx] and not (1'u64 shl bitIdx)

proc contains*(bs: BitSet, bitIndex: int): bool =
  ## Check if bit at `bitIndex` is set.
  if bitIndex >= bs.capacity or bitIndex < 0:
    return false
  
  let wordIdx = bitIndex shr 6
  let bitIdx = bitIndex and 63
  if wordIdx >= bs.words.len:
    return false
  
  (bs.words[wordIdx] and (1'u64 shl bitIdx)) != 0

proc union*(dest: var BitSet, src: BitSet): bool =
  ## Union `src` into `dest` using bitwise OR. Returns true if `dest` was modified.
  var changed = false
  
  # Expand dest if needed
  if src.capacity > dest.capacity:
    let newCapacity = src.capacity
    let newWordCount = (newCapacity + 63) div 64
    dest.words.setLen(newWordCount)
    dest.capacity = newCapacity
  
  # Perform word-level OR
  let minWords = min(dest.words.len, src.words.len)
  for i in 0 ..< minWords:
    let oldWord = dest.words[i]
    dest.words[i] = oldWord or src.words[i]
    if dest.words[i] != oldWord:
      changed = true
  
  result = changed

proc intersect*(dest: var BitSet, src: BitSet) =
  ## Intersect `dest` with `src` using bitwise AND.
  let minWords = min(dest.words.len, src.words.len)
  for i in 0 ..< minWords:
    dest.words[i] = dest.words[i] and src.words[i]
  
  # Clear remaining words in dest if src is smaller
  if dest.words.len > src.words.len:
    for i in src.words.len ..< dest.words.len:
      dest.words[i] = 0

proc clear*(bs: var BitSet) =
  ## Clear all bits (set all to 0).
  for i in 0 ..< bs.words.len:
    bs.words[i] = 0

proc `==`*(a, b: BitSet): bool =
  ## Check if two BitSets are equal.
  # Compare word by word
  let maxWords = max(a.words.len, b.words.len)
  for i in 0 ..< maxWords:
    let aWord = if i < a.words.len: a.words[i] else: 0'u64
    let bWord = if i < b.words.len: b.words[i] else: 0'u64
    if aWord != bWord:
      return false
  true

proc hash*(bs: BitSet): Hash =
  ## Hash a BitSet for use in tables.
  result = Hash(0)
  for word in bs.words:
    if word != 0:  # Only hash non-zero words for efficiency
      result = result !& hash(word)
  result = !$result

iterator items*(bs: BitSet): int =
  ## Iterate over all set bit indices.
  for wordIdx in 0 ..< bs.words.len:
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
