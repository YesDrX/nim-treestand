##
##  This file is included in generated parsers to provide common utilities
## 
##  * When a parser is generated, it will include this file `include treestand/parser_runtime`
##

{.push warning[UnusedImport]: off.}
import std/[os, strformat]
{.pop.}

# Helper to get symbol name for debugging
proc symbolName*(sym: parser_types.Symbol): string =
  ## Get the name of a symbol for debugging
  case sym.kind
  of skTerminal:
    if sym.terminalIndex >= 0 and sym.terminalIndex < terminalSymbolNames.len:
      return terminalSymbolNames[sym.terminalIndex]
    else:
      return "<unknown terminal " & $sym.terminalIndex & ">"
  of skNonTerminal:
    if sym.nonTerminalIndex >= 0 and sym.nonTerminalIndex < nonTerminalSymbolNames.len:
      return nonTerminalSymbolNames[sym.nonTerminalIndex]
    else:
      return "<unknown non-terminal " & $sym.nonTerminalIndex & ">"
  of skError:
    return "<error>"

proc isNamed*(sym: parser_types.Symbol): bool =
  ## Check if a symbol represents a named node
  case sym.kind
  of skTerminal:
    if sym.terminalIndex >= 0 and sym.terminalIndex < terminalSymbolMetadata.len:
      return terminalSymbolMetadata[sym.terminalIndex].named
    return false
  of skNonTerminal:
    if sym.nonTerminalIndex >= 0 and sym.nonTerminalIndex < nonTerminalSymbolMetadata.len:
      return nonTerminalSymbolMetadata[sym.nonTerminalIndex].named
    return true
  of skError:
    return false

# Pretty printing for parse nodes
proc `$`*(node: ParseNode): string =
  ## Convert parse node to string representation
  proc internalRec(n: ParseNode, level: int): string =
    if n == nil: return "nil"
    let indent = repeat("  ", level)
    let name = symbolName(n.symbol)
    
    if n.children.len == 0:
      # Leaf node
      # Hide regex patterns (starting with /) as they are noise
      let showName = not name.startsWith("/")
      
      var line = indent
      if showName:
        line.add(name)
        
      if n.token.text.len > 0:
        if showName: line.add(" ")
        line.add(n.token.text.escape)
      elif not showName:
        # If we hid the name and there's no text, show name anyway to avoid empty line?
        # Typically regex tokens match something, so text > 0.
        # But if it's epsilon or empty match?
        line.add(name) 
      
      line.add(" [" & $n.token.startPos & "-" & $n.token.endPos & "]\n")
      result = line
    else:
      # Non-leaf node
      result = indent & name & "\n"
      for child in n.children:
        result.add(internalRec(child, level + 1))
        
  internalRec(node, 0)

proc toSExpr*(node: ParseNode, indent: int = 0): string =
  ## Convert parse node to pretty S-expression format
  if node == nil:
    return "nil"
  
  let name = symbolName(node.symbol)
  let prefix = "  ".repeat(indent)
  
  if node.children.len == 0:
    # Leaf node
    if node.token.text.len > 0:
      return prefix & "(" & name & " \"" & node.token.text & "\")"
    else:
      return prefix & "(" & name & ")"
  else:
    # Non-leaf node
    var lines = @[prefix & "(" & name]
    for child in node.children:
      if child != nil:
        lines.add(toSExpr(child, indent + 1))
    lines.add(prefix & ")")
    return lines.join("\n")

proc walkTree*(node: ParseNode, visit: proc(n: ParseNode) {.closure.}) =
  ## Walk the parse tree in pre-order, calling visit for each node
  if node == nil:
    return
  visit(node)
  for child in node.children:
    walkTree(child, visit)

proc findNodes*(node: ParseNode, predicate: proc(n: ParseNode): bool {.closure.}): seq[ParseNode] =
  ## Find all nodes matching the predicate
  result = @[]
  walkTree(node) do (n: ParseNode):
    if predicate(n):
      result.add(n)

proc findNodesBySymbol*(node: ParseNode, symbolNameToFind: string): seq[ParseNode] =
  ## Find all nodes with a specific symbol name
  findNodes(node) do (n: ParseNode) -> bool:
    symbolName(n.symbol) == symbolNameToFind

# GLR Runtime
proc runGenericGLR*(parser: var Parser, raiseOnFail: bool = false): ParseNode =
  var activeStacks = parser.stacks
  var successes: seq[ParseNode] = @[]
  
  # Track recovered nodes (ERROR nodes and successfully parsed children) for building final tree
  # This allows us to include ERROR nodes in the result even after error recovery
  var recoveredNodes: seq[ParseNode] = @[]
  
  # Recovery point tracking: save lexer position when at state 0 (good, stable position)
  # On error, we'll rewind to this position and try skipping minimal tokens
  var lastGoodLexerPos = parser.lexer.pos
  var lastGoodLexerRow = parser.lexer.row
  var lastGoodLexerCol = parser.lexer.col
  
  # Limit iterations to prevent infinite loops in bad grammars
  # But infinite loop detection should happen via state logic ideally.
  
  while true:
    # Update recovery point when we're at state 0 with a valid stack
    if activeStacks.len > 0 and activeStacks[0].len > 0:
      let topState = activeStacks[0][^1].state
      if topState == 0:
        lastGoodLexerPos = parser.lexer.pos
        lastGoodLexerRow = parser.lexer.row
        lastGoodLexerCol = parser.lexer.col
        debugEchoMsg fmt"[Recovery] Saved recovery point at pos {lastGoodLexerPos}"
    
    var pendingShifts: seq[tuple[stackIdx: int, newState: uint32, isExtra: bool]] = @[]
    
    # Process each active stack
    # We use a queue to handle reductions (which don't consume token)
    # until we hit a Shift, Accept, Error, or need to Wait (Split?)
    # Actually, we process one step. If reduce, we get a new stack configuration.
    # We must process all configurations until they all want to Shift or Die.
    
    var queue: seq[seq[tuple[state: int, node: ParseNode]]] = activeStacks
    var shiftableStacks: seq[seq[tuple[state: int, node: ParseNode]]] = @[]
    
    # Currently `parser.lookahead` is already set.
    
    var i = 0
    while i < queue.len:
      let currentStack = queue[i]
      inc i
      
      let state = currentStack[^1].state
      if state >= parseTableIndex.len: continue

      debugEchoMsg fmt"queue.len: {queue.len}, i: {i}, currentStack.len: {currentStack.len}"
      debugEchoMsg fmt"Processing state: {state} with lookahead: (kind: {parser.lookahead.kind.kind}, terminalIndex: {parser.lookahead.kind.terminalIndex}, text: {parser.lookahead.text})"
      
      # Lookup actions (replacing findAction helper)
      var actions: seq[parser_types.ParseAction] = @[]

      let idx = parseTableIndex[state]
      for k in 0 ..< idx.actionLen:
        let (s, act) = parseTableActions[idx.actionStart + k]
        if s == parser.lookahead.kind:
           actions.add(act)
      
      # Implicitly handle external extras if no explicit action found
      if actions.len == 0 and parser.lookahead.kind.kind == parser_types.skTerminal:
        if parser.lookahead.kind.terminalIndex.int16 in externalExtraTokens:
          actions.add(parser_types.ParseAction(kind: parser_types.pakShiftExtra))
      
      if actions.len == 0:
        actions.add(parser_types.ParseAction(kind: parser_types.pakError))

      
      proc processAction(act: ParseAction, lookahead: Token) =
        case act.kind
        of pakSplit:
          for sub in act.splitActions:
            processAction(sub, lookahead)
        of pakShift:
          # Record that this stack wants to shift
          shiftableStacks.add(currentStack)
          pendingShifts.add((stackIdx: shiftableStacks.len - 1, newState: act.shiftState, isExtra: false))
        of pakReduce:
          # Perform reduction
          var newStack = currentStack
          var children: seq[ParseNode] = @[]
          for k in 0 ..< act.reduceCount.int:
             let item = newStack.pop()
             children.add(item.node)
          # Manual reverse to avoid std/algorithm linker issues
          for j in 0 ..< children.len div 2:
             let tmp = children[j]
             children[j] = children[children.len - 1 - j]
             children[children.len - 1 - j] = tmp
          
          let startPos = if children.len > 0: children[0].startPos else: lookahead.startPos
          let endPos = if children.len > 0: children[^1].endPos else: lookahead.endPos
          let startPoint = if children.len > 0: children[0].startPoint else: lookahead.startPoint
          let endPoint = if children.len > 0: children[^1].endPoint else: lookahead.endPoint
          
          let newNode = ParseNode(symbol: act.reduceSymbol, children: children, startPos: startPos, endPos: endPos, startPoint: startPoint, endPoint: endPoint)
          
          let currentState = newStack[^1].state
          if currentState < parseTableIndex.len:
            # Find Goto
            var gotoState = -1
            let gIdx = parseTableIndex[currentState]
            when parseTableGotos.len > 0:
              for k in 0 ..< gIdx.gotoLen:
                # echo parseTableGotos[gIdx.gotoStart + k]
                let (s, gs) = parseTableGotos[gIdx.gotoStart + k]
                if s == act.reduceSymbol:
                  gotoState = gs.int
                  break
            if gotoState >= 0:
              newStack.add((gotoState, newNode))
              # debugEchoMsg "    -> Reduced to state ", gotoState, ", adding back to queue"
              queue.add(newStack) # Add back to queue to continue
        of pakAccept:
          if currentStack.len > 1:
             successes.add(currentStack[^1].node)
          else:
             successes.add(ParseNode(symbol: terminal(0), children: @[], startPos:0, endPos:0, startPoint: Point(row:0, column:0), endPoint: Point(row:0, column:0)))
        of pakError:
          discard
        of pakShiftExtra:
          # Handle Shift Extra: Consume token, but stay in same state, effectively skipping it in parse stack logic
          shiftableStacks.add(currentStack)
          # We use currentState as dummy newState, but isExtra=true handles behavior
          pendingShifts.add((stackIdx: shiftableStacks.len - 1, newState: uint32(state), isExtra: true))
      
      for act in actions:
        processAction(act, parser.lookahead)
    
    # End of Phase 1
    if successes.len > 0:
      # If we have recovered nodes (ERROR nodes from error recovery),
      # we need to incorporate them into the final tree
      if recoveredNodes.len > 0:
        let successTree = successes[0]
        
        # Build a new program node that includes both ERROR nodes and successfully parsed content
        # The successTree should be the program node; we need to inject recoveredNodes as children
        var allChildren: seq[ParseNode] = @[]
        
        # Add recovered nodes (ERROR nodes) first
        allChildren.add(recoveredNodes)
        
        # Then add children from the success tree (if any)
        if successTree != nil and successTree.children.len > 0:
          allChildren.add(successTree.children)
        
        # Create new program node with all children
        let finalTree = ParseNode(
          symbol: successTree.symbol,
          children: allChildren,
          startPos: if allChildren.len > 0: allChildren[0].startPos else: 0,
          endPos: if allChildren.len > 0: allChildren[^1].endPos else: 0,
          startPoint: if allChildren.len > 0: allChildren[0].startPoint else: Point(row:0, column:0),
          endPoint: if allChildren.len > 0: allChildren[^1].endPoint else: Point(row:0, column:0)
        )
        return finalTree
      else:
        return successes[0] # No error recovery, return normal result
    
    # OLD: Just die if no shifts
    # if pendingShifts.len == 0:
    #   # No shifts, no successes -> Error
    #   # echo "[GLR] All ", queue.len, " paths deadended on lookahead ", symbolName(parser.lookahead.kind), " (", parser.lookahead.text, ")"
    #   # for i, s in queue:
    #   #    echo "  Stack ", i, " top state: ", s[^1].state
    #   raise newException(ValueError, "Parse error: All paths failed")

    # NEW: Skip garbage tokens and mark as an Error node
    if pendingShifts.len == 0:
      # ========================================================================
      # Old behavior: Just die if no shifts
      # ========================================================================
      if raiseOnFail:
        raise newException(ValueError, "Parse error: All paths failed")

      # ========================================================================
      # ERROR RECOVERY: Skip Strategy
      # ========================================================================
      
      let badToken = parser.lookahead
      
      # 1. EOF Check (Hard Failure / Force Accept)
      if badToken.kind.kind == skTerminal and badToken.kind.terminalIndex == 0:
         # ... (Use the EOF Fix from previous answer) ...
         # [Paste the EOF force-accept logic here]
         discard

      # ============================================================
      # NEW: Strategy 2 - Stack Unwinding (Context Recovery)
      # ============================================================
      # Before skipping the token, check if a PARENT state can handle it.
      # This allows us to "break out" of a bad rule (like an unclosed object).
      
      var unwound = false
      
      # We only try this on the first active stack for simplicity in this heuristic
      if activeStacks.len > 0:
        let stack = activeStacks[0]
        
        # Iterate backwards from the parent of current state down to root
        # i = index of the candidate state to resume from
        for i in countdown(stack.len - 2, 0): 
            let ancestorState = stack[i].state
           
            # Check if 'ancestorState' accepts 'badToken'
            var accepts = false
            if ancestorState < parseTableIndex.len:
                let idx = parseTableIndex[ancestorState]
                for k in 0 ..< idx.actionLen:
                    let (sym, _) = parseTableActions[idx.actionStart + k]
                    if sym == badToken.kind:
                      accepts = true
                      break
           
            if accepts:
              debugEchoMsg fmt"[Recovery] Unwinding stack to state {ancestorState} to handle '{badToken.text}'"
             
              # 1. Collect all nodes that are being dropped (from i+1 to top)
              var droppedNodes: seq[ParseNode] = @[]
              for j in (i + 1) ..< stack.len:
                if stack[j].node != nil:
                  droppedNodes.add(stack[j].node)
             
              # 2. Wrap them in an ERROR node
              let errorNode = ParseNode(
                  symbol: Symbol(kind: skError),
                  children: droppedNodes,
                  token: badToken, # Mark the location where we gave up
                  startPos: if droppedNodes.len > 0: droppedNodes[0].startPos else: badToken.startPos,
                  endPos: if droppedNodes.len > 0: droppedNodes[^1].endPos else: badToken.endPos,
                  startPoint: if droppedNodes.len > 0: droppedNodes[0].startPoint else: badToken.startPoint,
                  endPoint: if droppedNodes.len > 0: droppedNodes[^1].endPoint else: badToken.endPoint
                )
              
              # 3. Add ERROR node to recovered nodes list (will be included in final tree)
              recoveredNodes.add(errorNode)
              
              # 4. Unwind the stack to the ancestor state (don't add ERROR to stack)
              # Create the new stack: [0..i] (just the unwound states, ERROR is tracked separately)
              var newStack = stack[0..i]
              
              # 5. Update Parser
              activeStacks = @[newStack]
              parser.stacks = activeStacks
              # 4. RECOVERY POINT STRATEGY: Instead of just unwinding stack and skipping forward,
              # rewind lexer to the last good position (saved recovery point) and try skipping
              # minimal tokens from there (1, 2, 3...) until we can parse successfully
              
              debugEchoMsg fmt"[Recovery] Rewinding lexer from pos {parser.lexer.pos} to recovery point {lastGoodLexerPos}"
              
              # Rewind lexer to recovery point
              parser.lexer.pos = lastGoodLexerPos
              parser.lexer.row = lastGoodLexerRow  
              parser.lexer.col = lastGoodLexerCol
              
              # Try skipping 0, 1, 2, 3... tokens from recovery point until we find a valid continuation
              # Starting with 0 means we first try WITHOUT skipping, preserving as many tokens as possible
              # Limit attempts to avoid infinite loops
              var tokensToSkip = 0
              var foundValidContinuation = false
              let maxSkipAttempts = 10
              
              while tokensToSkip <= maxSkipAttempts and not foundValidContinuation:
                debugEchoMsg fmt"[Recovery] Attempting to skip {tokensToSkip} token(s) from recovery point"
                
                # Reset lexer to recovery point for this attempt
                parser.lexer.pos = lastGoodLexerPos
                parser.lexer.row = lastGoodLexerRow
                parser.lexer.col = lastGoodLexerCol
                
                # Prepare valid external symbols for state 0
                var recoveryValidExternal = externalExtraTokens
                if 0 < parseTableIndex.len:
                  let idx = parseTableIndex[0]
                  for k in 0 ..< idx.actionLen:
                    let (sym, _) = parseTableActions[idx.actionStart + k]
                    if sym.kind == skTerminal and sym.terminalIndex >= externalTokenBase:
                      recoveryValidExternal.incl(sym.terminalIndex.int16)
                
                let lexState = parseTableIndex[0].lexState
                
                # Skip N tokens
                for skipCount in 1..tokensToSkip:
                  let skippedToken = parser.lexer.nextToken(recoveryValidExternal, lexState)
                  debugEchoMsg fmt"[Recovery]   Skipped token #{skipCount}: '{skippedToken.text}'"
                  if skippedToken.text.len == 0:
                    #  Hit EOF, stop trying
                    break
                
                # Now try to get the next token and check if it's shiftable from state 0
                parser.lookahead = parser.lexer.nextToken(recoveryValidExternal, lexState)
                
                if parser.lookahead.text.len == 0:
                  # Hit EOF, stop trying
                  debugEchoMsg "[Recovery] Hit EOF, stopping skip attempts"
                  break
                
                # Check if this token can be shifted/accepted from state 0
                var canProgress = false
                if 0 < parseTableIndex.len:
                  let idx = parseTableIndex[0]
                  for k in 0 ..< idx.actionLen:
                    let (sym, act) = parseTableActions[idx.actionStart + k]
                    if sym == parser.lookahead.kind:
                      if act.kind == pakShift or act.kind == pakAccept or act.kind == pakShiftExtra:
                        canProgress = true
                        debugEchoMsg fmt"[Recovery] Found valid continuation after skipping {tokensToSkip} tokens: '{parser.lookahead.text}' can {act.kind}"
                        foundValidContinuation = true
                        break
                
                if not foundValidContinuation:
                  tokensToSkip += 1
              
              # Now set up the stack to resume from state 0
              var recoveryStack = stack[0..0]  # Just keep root state
              activeStacks = @[recoveryStack]
              parser.stacks = activeStacks
              
              unwound = true
              break
        
        if unwound:
          continue # Restart loop with new stack and lookahead

      # ============================================================
      # Strategy 3: Skip Token (Fallback)
      # ============================================================
      # If Unwinding didn't help (or wasn't possible), consume the garbage.
      
      # Check for EOF - if we hit EOF and can't process it, we're done
      if badToken.text.len == 0:
        debugEchoMsg "[Recovery] Hit EOF that cannot be processed, forcing accept"
        # Force an accept by creating a minimal success tree
        if activeStacks.len > 0 and activeStacks[0].len > 0:
          let rootNode = activeStacks[0][0].node
          if rootNode != nil:
            successes.add(rootNode)
        break
      
      debugEchoMsg fmt"[Recovery] Skipping unexpected token: {badToken.text}"
      
      let errorNode = ParseNode(
        symbol: Symbol(kind: skError), 
        children: @[],                 
        token: badToken,               
        startPos: badToken.startPos,
        endPos: badToken.endPos,
        startPoint: badToken.startPoint,
        endPoint: badToken.endPoint
      )

      var recoveredStacks: seq[seq[tuple[state: int, node: ParseNode]]] = @[]
      for stack in activeStacks:
         var newStack = stack
         newStack.add((stack[^1].state, errorNode)) 
         recoveredStacks.add(newStack)
      
      activeStacks = recoveredStacks
      parser.stacks = activeStacks

      # Advance Lexer (Manually)
      var recoveryValidExternal = externalExtraTokens
      for stack in activeStacks:
        let s = stack[^1].state
        if s < parseTableIndex.len:
           let idx = parseTableIndex[s]
           for k in 0 ..< idx.actionLen:
             let (sym, _) = parseTableActions[idx.actionStart + k]
             if sym.kind == skTerminal and sym.terminalIndex >= externalTokenBase:
                recoveryValidExternal.incl(sym.terminalIndex.int16)

      let lexState = parseTableIndex[activeStacks[0][^1].state].lexState
      parser.lookahead = parser.lexer.nextToken(recoveryValidExternal, lexState)

      continue
          

    # Phase 2: Shift
    # Consume token
    let token = parser.lookahead
    let node = ParseNode(symbol: token.kind, token: token, children: @[], startPos: token.startPos, endPos: token.endPos, startPoint: token.startPoint, endPoint: token.endPoint)
    
    # Calculate next token (Lookahead)
    # We need to union valid external symbols from ALL next states
    var validExternal: set[int16] = externalExtraTokens
    
    var nextActiveStacks: seq[seq[tuple[state: int, node: ParseNode]]] = @[]
    
    for shift in pendingShifts:
       var stack = shiftableStacks[shift.stackIdx]
       if not shift.isExtra:
          stack.add((shift.newState.int, node))
          nextActiveStacks.add(stack)
          # Accumulate valid externals from TARGET state
          if shift.newState.int < parseTableIndex.len:
            let idx = parseTableIndex[shift.newState.int]
            for k in 0 ..< idx.actionLen:
              let (sym, _) = parseTableActions[idx.actionStart + k]
              if sym.kind == skTerminal:
                if sym.terminalIndex >= externalTokenBase:
                  validExternal.incl(sym.terminalIndex.int16)
       else:
         # Extra: Keep stack as is (state doesn't change)
         nextActiveStacks.add(stack)
         
         # Accumulate valid externals from CURRENT state
         if stack[^1].state < parseTableIndex.len:
            let idx = parseTableIndex[stack[^1].state]
            for k in 0 ..< idx.actionLen:
              let (sym, _) = parseTableActions[idx.actionStart + k]
              if sym.kind == skTerminal:
                if sym.terminalIndex >= externalTokenBase:
                  validExternal.incl(sym.terminalIndex.int16)
           
    
    activeStacks = nextActiveStacks
    parser.stacks = activeStacks
    
    # Get next token
    # We need `lexicalGrammar` to know external base?
    # Or just use `parseTableEntries[0].lexState`? No, lex state depends on state.
    # GLR requires all heads to agree on lex state?
    # Tree-sitter handles this.
    # For now, use lex state of first head.
    let firstState = activeStacks[0][^1].state
    let lexState = parseTableIndex[firstState].lexState
    
    var excludedExternals: set[int16] = {}
    while true:
       let currentValid = validExternal - excludedExternals
       parser.lookahead = parser.lexer.nextToken(currentValid, lexState)
       
       # Prevent infinite loops with zero-width extras (e.g. empty comments)
       if parser.lookahead.endPos == parser.lookahead.startPos:
          # Check if this token is treated as an Extra in the current state
          # We check the first active state as a representative (extras are usually global)
          var action = parser_types.ParseAction(kind: parser_types.pakError)
          if firstState < parseTableIndex.len:
            let idx = parseTableIndex[firstState]
            for k in 0 ..< idx.actionLen:
              let (s, act) = parseTableActions[idx.actionStart + k]
              if s == parser.lookahead.kind:
                  action = act
                  break
            
            # Implicit validation for loop protection
            if action.kind == parser_types.pakError and parser.lookahead.kind.kind == parser_types.skTerminal:
               if parser.lookahead.kind.terminalIndex.int16 in externalExtraTokens:
                 action = parser_types.ParseAction(kind: parser_types.pakShiftExtra)
          
          if action.kind == pakShiftExtra:
             # Zero-width extra would cause an infinite loop (consume but don't advance, don't change state)
             # So we blacklist it and try to find another token
             if parser.lookahead.kind.kind == skTerminal:
                excludedExternals.incl(parser.lookahead.kind.terminalIndex.int16)
                continue
       break

# ------------------------------------------------------------------------------
# User Convenience API
# ------------------------------------------------------------------------------

proc kind*(node: ParseNode): string =
  ## Get the node kind (symbol name) as a string
  if node == nil: return "nil"
  return symbolName(node.symbol)

proc text*(node: ParseNode): string =
  ## Get the text content of the node
  ## For terminal nodes, this returns the token text
  ## For non-terminal nodes, this concatenates the text of all children
  if node == nil: return ""
  if node.children.len == 0:
    return node.token.text
  
  result = ""
  for child in node.children:
    result.add(text(child))

proc child*(node: ParseNode, index: int): ParseNode =
  ## Get child at index, returning nil if out of bounds
  if node == nil or index < 0 or index >= node.children.len:
    return nil
  return node.children[index]

proc childCount*(node: ParseNode): int =
  ## Get number of children
  if node == nil: return 0
  return node.children.len

proc child*(node: ParseNode, name: string): ParseNode =
  ## Find first child with the given symbol name (e.g. "assign", "ident")
  if node == nil: return nil
  for c in node.children:
    if symbolName(c.symbol) == name:
      return c
  return nil

proc capture*(node: ParseNode): string =
  ## Syntactic sugar for text(node)
  text(node)

iterator children*(node: ParseNode): ParseNode =
  ## Iterate over all children
  if node != nil:
    for child in node.children:
      yield child

iterator namedChildren*(node: ParseNode): ParseNode =
  ## Iterate over named children only
  if node != nil:
    for child in node.children:
      if isNamed(child.symbol):
        yield child

proc namedChild*(node: ParseNode, index: int): ParseNode =
  ## Get named child at index, returning nil if out of bounds
  if node == nil: return nil
  var current = 0
  for child in node.namedChildren:
    if current == index:
      return child
    inc current
  return nil

proc namedChildCount*(node: ParseNode): int =
  ## Get number of named children
  if node == nil: return 0
  var count = 0
  for _ in node.namedChildren:
    inc count
  return count

proc hasError*(node: ParseNode): bool =
  ## Check if the node or any of its children is an error node
  if node == nil: return false
  if node.symbol.kind == skTerminal and node.symbol.terminalIndex == -1: # assuming error token logic if applicable? 
     # Actually standard tree-sitter uses a specific symbol for ERROR. 
     # In our runtime, we might not have a specific 'ERROR' symbol checked this way yet.
     # But usually error nodes are created during GLR failure or specific error actions.
     # Based on `parser_runtime.nim` lines 203, pakError does discard.
     # GLR returns "nil" (or exception) on full failure, but generated error nodes?
     # Checking `symbolName`: if it returns "ERROR", that's it.
     return false
  
  # Check if symbol kind name includes "ERROR" maybe?
  let k = kind(node)
  if k == "ERROR" or k == "MISSING":
    return true
    
  for child in node.children:
    if hasError(child):
      return true
  return false

proc edit*(node: ParseNode, edit: InputEdit) =
  ## Edit the node to reflect a change in the source code
  if node == nil: return
  
  let oldEndPos = edit.startByte + edit.oldEndByte
  let newEndPos = edit.startByte + edit.newEndByte
  let delta = newEndPos - oldEndPos
  
  # Check if node is after the edit
  if node.startPos >= oldEndPos:
    # Shift node
    node.startPos += delta
    node.endPos += delta
    
    # Update Points
    # Use int for arithmetic to handle negative deltas/deletions safely
    let dRow = edit.newEndPoint.row.int - edit.oldEndPoint.row.int
    # dCol is strictly relevant only for nodes on the same line as the edit end
    
    # Start Point
    if node.startPoint.row == edit.oldEndPoint.row:
       node.startPoint.column = (node.startPoint.column.int - edit.oldEndPoint.column.int + edit.newEndPoint.column.int).uint32
       node.startPoint.row = edit.newEndPoint.row
    elif node.startPoint.row > edit.oldEndPoint.row:
       node.startPoint.row = (node.startPoint.row.int + dRow).uint32
       
    # End Point
    if node.endPoint.row == edit.oldEndPoint.row:
       node.endPoint.column = (node.endPoint.column.int - edit.oldEndPoint.column.int + edit.newEndPoint.column.int).uint32
       node.endPoint.row = edit.newEndPoint.row
    elif node.endPoint.row > edit.oldEndPoint.row:
       node.endPoint.row = (node.endPoint.row.int + dRow).uint32
      
    # Recurse children?
    # If node is fully AFTER, its children are also AFTER.
    # We must traverse to update them too in current model where points are stored on each node.
    for child in node.children:
      edit(child, edit)
      
  elif node.endPos > edit.startByte:
    # Node overlaps or contains the edit
    # Update end position
    node.endPos += delta
    
    # Update end point
    let dRow = edit.newEndPoint.row.int - edit.oldEndPoint.row.int
    
    if node.endPoint.row == edit.oldEndPoint.row:
       node.endPoint.column = (node.endPoint.column.int - edit.oldEndPoint.column.int + edit.newEndPoint.column.int).uint32
       node.endPoint.row = edit.newEndPoint.row
    elif node.endPoint.row > edit.oldEndPoint.row:
       node.endPoint.row = (node.endPoint.row.int + dRow).uint32

    # Recurse to children
    for child in node.children:
      edit(child, edit)

# ------------------------------------------------------------------------------
# Corpus Generation
# ------------------------------------------------------------------------------
proc writeCorpus*(filename: string, testName: string, input: string, tree: ParseNode = nil, expectedSExpr: string = "") =
  ## Write or append a test case to a corpus file
  ## If `tree` is provided, it is converted to S-expression as expected output.
  ## If `expectedSExpr` is provided, it is used directly.
  
  let fileMode = if fileExists(filename): fmAppend else: fmWrite
  let f = open(filename, fileMode)
  defer: f.close()
  
  if fileMode == fmAppend:
    f.write("\n")
    
  f.writeLine("==================")
  f.writeLine(testName)
  f.writeLine("==================")
  f.writeLine(input)
  f.writeLine("---")
  
  var output = expectedSExpr
  if output.len == 0 and tree != nil:
    output = toSExpr(tree)
    
  if output.len > 0:
    f.writeLine(output)
    if output[^1] != '\n':
      f.write("\n")
  else:
    f.writeLine("")
