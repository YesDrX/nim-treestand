## NFA (Non-deterministic Finite Automaton) for lexical analysis
when defined(profiler):
  import nimprof

import options, sets, std/algorithm

type
  CharacterRange* = object
    start*: uint32
    `end`*: uint32  # exclusive

  CharacterSet* = object
    ranges*: seq[CharacterRange]

  NfaStateKind* = enum
    nskAdvance
    nskSplit
    nskAccept

  NfaState* = object
    case kind*: NfaStateKind
    of nskAdvance:
      advanceChars*: CharacterSet
      advanceStateId*: uint32
      advanceIsSep*: bool
      advancePrecedence*: int32
    of nskSplit:
      splitLeftState*: uint32
      splitRightState*: uint32
    of nskAccept:
      acceptVariableIndex*: int
      acceptPrecedence*: int32

  Nfa* = object
    states*: seq[NfaState]

  NfaCursor* = object
    stateIds*: seq[uint32]
    nfa*: ptr Nfa

  NfaTransition* = object
    characters*: CharacterSet
    isSeparator*: bool
    precedence*: int32
    states*: seq[uint32]

const
  EndChar* = uint32(0x110000)  # Unicode max + 1

proc emptyCharacterSet*(): CharacterSet =
  CharacterSet(ranges: @[])

proc fromChar*(c: char): CharacterSet =
  let code = uint32(c)
  CharacterSet(ranges: @[CharacterRange(start: code, `end`: code + 1)])

proc fromRange*(first, last: char): CharacterSet =
  var f = uint32(first)
  var l = uint32(last)
  if f > l:
    swap(f, l)
  CharacterSet(ranges: @[CharacterRange(start: f, `end`: l + 1)])

proc contains*(cs: CharacterSet, c: char): bool =
  let code = uint32(c)
  for r in cs.ranges:
    if code >= r.start and code < r.`end`:
      return true
  false

proc intersects*(a, b: CharacterSet): bool =
  ## Fast check if two character sets overlap (no allocation)
  ## This is used heavily in conflict resolution
  for r1 in a.ranges:
    for r2 in b.ranges:
      if max(r1.start, r2.start) < min(r1.`end`, r2.`end`):
        return true
  return false

proc intersection*(a, b: CharacterSet): CharacterSet =
  ## Compute intersection of two character sets
  ## Returns a new CharacterSet with ranges that overlap in both sets
  var ranges: seq[CharacterRange] = @[]
  for r1 in a.ranges:
    for r2 in b.ranges:
      let start = max(r1.start, r2.start)
      let stop = min(r1.`end`, r2.`end`)
      if start < stop:
        ranges.add(CharacterRange(start: start, `end`: stop))
  CharacterSet(ranges: ranges)


proc negate*(cs: CharacterSet): CharacterSet =
  ## Create a character set containing all characters NOT in this set
  var negated = CharacterSet(ranges: @[])
  var previousEnd: uint32 = 0
  var i = 0
  var ranges = cs.ranges
  
  while i < ranges.len:
    var range = ranges[i]
    let start = previousEnd
    previousEnd = range.`end`
    if start < range.start:
      negated.ranges.add(CharacterRange(start: start, `end`: range.start))
      inc i
    else:
      ranges.delete(i)
  
  if previousEnd < EndChar:
    negated.ranges.add(CharacterRange(start: previousEnd, `end`: EndChar))
  
  negated

proc newNfa*(): Nfa =
  Nfa(states: @[])

proc addState*(nfa: var Nfa, state: NfaState): uint32 =
  result = uint32(nfa.states.len)
  nfa.states.add(state)

proc epsilonClosure(nfa: ptr Nfa, states: openArray[uint32]): seq[uint32] =
  var visited = initHashSet[uint32]()
  var stack = newSeq[uint32]()
  var resultSeq = newSeq[uint32]()
  
  for s in states:
    if s notin visited:
      visited.incl(s)
      stack.add(s)
      resultSeq.add(s)
      
  var i = 0
  while i < stack.len:
    let s = stack[i]
    i += 1
    
    if s < uint32(nfa.states.len):
      let state = nfa.states[s.int]
      if state.kind == nskSplit:
        if state.splitLeftState notin visited:
          visited.incl(state.splitLeftState)
          stack.add(state.splitLeftState)
          resultSeq.add(state.splitLeftState)
        if state.splitRightState notin visited:
          visited.incl(state.splitRightState)
          stack.add(state.splitRightState)
          resultSeq.add(state.splitRightState)
  
  # Note: assuming states seq doesn't need to be sorted for correctness inside nfa ops,
  # but nice for determinism.
  resultSeq.sort()
  return resultSeq

proc newNfaCursor*(nfa: ptr Nfa): NfaCursor =
  result = NfaCursor(stateIds: @[], nfa: nfa)
  result.stateIds = epsilonClosure(nfa, @[0u32])

proc reset*(cursor: var NfaCursor) =
  cursor.stateIds = epsilonClosure(cursor.nfa, @[0u32])

proc isInAcceptState*(cursor: NfaCursor): bool =
  for stateId in cursor.stateIds:
    if stateId < uint32(cursor.nfa.states.len):
      let state = cursor.nfa.states[stateId.int]
      if state.kind == nskAccept:
        return true
  false

proc getAcceptingVariable*(cursor: NfaCursor): Option[int] =
  var bestPrecedence = low(int32)
  var bestVariable: Option[int] = none(int)
  
  for stateId in cursor.stateIds:
    if stateId < uint32(cursor.nfa.states.len):
      let state = cursor.nfa.states[stateId.int]
      if state.kind == nskAccept:
        if state.acceptPrecedence > bestPrecedence:
          bestPrecedence = state.acceptPrecedence
          bestVariable = some(state.acceptVariableIndex)
  
  bestVariable

proc advance*(cursor: var NfaCursor, c: char, isSeparator: bool): seq[uint32] =
  var nextStates: seq[uint32] = @[]
  var visited = initHashSet[uint32]()
  
  for stateId in cursor.stateIds:
    if stateId < uint32(cursor.nfa.states.len):
      let state = cursor.nfa.states[stateId.int]
      case state.kind
      of nskAdvance:
        if c in state.advanceChars and state.advanceIsSep == isSeparator:
          if state.advanceStateId notin visited:
            visited.incl(state.advanceStateId)
            nextStates.add(state.advanceStateId)
      of nskSplit:
        if state.splitLeftState notin visited:
          visited.incl(state.splitLeftState)
          nextStates.add(state.splitLeftState)
        if state.splitRightState notin visited:
          visited.incl(state.splitRightState)
          nextStates.add(state.splitRightState)
      of nskAccept:
        discard
  
  cursor.stateIds = nextStates
  result = nextStates

proc completions*(cursor: var NfaCursor): seq[(int, int32)] =
  result = @[]
  for stateId in cursor.stateIds:
    if stateId < uint32(cursor.nfa.states.len):
      let state = cursor.nfa.states[stateId.int]
      if state.kind == nskAccept:
        result.add((state.acceptVariableIndex, state.acceptPrecedence))

proc transitions*(cursor: var NfaCursor): seq[NfaTransition] =
  result = @[]
  
  # We need to group transitions by character set
  # Tree-sitter does this by iterating all possible chars? No, it likely iterates the states.
  # If multiple states have transitions, we need to collect them.
  # But importantly, we need to produce Disjoint transitions if we want to mimic a DFA-like step?
  # Or does NfaCursor::transitions return NFA transitions directly?
  # Checking tree-sitter source: It returns NfaTransition which has `states: Vec<u32>`.
  # This implies it computes the "next states" for a given input.
  # This is effectively a single step of DFA construction / subset construction on the fly.
  
  # Grouping by character ranges is complex.
  # SImplified approach:
  # Collect all advance transitions.
  # Find all unique "boundaries" in the character sets.
  # Create a set of disjoint ranges.
  # For each disjoint range, find target states.
  
  var outgoing: seq[tuple[chars: CharacterSet, isSep: bool, state: uint32, prec: int32]] = @[]
  
  for stateId in cursor.stateIds:
    if stateId >= uint32(cursor.nfa.states.len): continue
    let state = cursor.nfa.states[stateId.int]
    
    if state.kind == nskAdvance:
      outgoing.add((state.advanceChars, state.advanceIsSep, state.advanceStateId, state.advancePrecedence))
  
  if outgoing.len == 0:
    return
  
  # Basic boundary finding
  var points = initHashSet[uint32]()
  for item in outgoing:
    for r in item.chars.ranges:
      points.incl(r.start)
      points.incl(r.`end`)
  
  var sortedPoints = newSeq[uint32]()
  for p in points: sortedPoints.add(p)
  sortedPoints.sort()
  
  for i in 0 ..< sortedPoints.len - 1:
    let start = sortedPoints[i]
    let `end` = sortedPoints[i+1]
    if start >= `end`: continue
    
    # Check this range against all outgoing
    # We might have separate transitions for separator vs non-separator?
    # Tree-sitter NfaTransition has `is_separator`.
    # If a range matches both separator and non-separator, we likely split them?
    # Or does `TokenConflictMap` handle that? 
    # Rust `NfaCursor::transitions`: "Computes the set of transitions... grouping by character set and separator status".
    
    # We'll group by (isSeparator)
    for isSep in [false, true]:
      var nextStates = initHashSet[uint32]()
      var maxPrecedence = low(int32)
      var hasTransition = false
      
      for item in outgoing:
        if item.isSep != isSep: continue
        
        # Check if range is in item.chars
        # Use proper range check for uint32
        var inRange = false
        for r in item.chars.ranges:
          if start >= r.start and start < r.`end`:
            inRange = true
            break
        
        if inRange:
          hasTransition = true
          nextStates.incl(item.state)
          if item.prec > maxPrecedence:
            maxPrecedence = item.prec
      
      if hasTransition:
         # Compute closure of nextStates
         var closureStates = initHashSet[uint32]()
         var stack = newSeq[uint32]()
         for s in nextStates: 
           closureStates.incl(s)
           stack.add(s)
           
         while stack.len > 0:
           let s = stack.pop()
           if s >= uint32(cursor.nfa.states.len): continue
           let st = cursor.nfa.states[s.int]
           if st.kind == nskSplit:
             if st.splitLeftState notin closureStates:
               closureStates.incl(st.splitLeftState)
               stack.add(st.splitLeftState)
             if st.splitRightState notin closureStates:
               closureStates.incl(st.splitRightState)
               stack.add(st.splitRightState)
         
         var stateSeq = newSeq[uint32]()
         for s in closureStates: stateSeq.add(s)
         stateSeq.sort()
         
         result.add(NfaTransition(
           characters: CharacterSet(ranges: @[CharacterRange(start: start, `end`: `end`)]),
           isSeparator: isSep,
           precedence: maxPrecedence,
           states: stateSeq
         ))

proc transitionChars*(cursor: var NfaCursor): seq[(CharacterSet, bool)] =
  ## Returns iterator of (chars, isSeparator) for all transitions
  # Just used for simple checks
  result = @[]
  let trans = cursor.transitions()
  for t in trans:
    result.add((t.characters, t.isSeparator))

proc setStates*(cursor: var NfaCursor, bucket: seq[uint32]) =
  cursor.stateIds = epsilonClosure(cursor.nfa, bucket)
