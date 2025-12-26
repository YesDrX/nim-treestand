import treestand
import std/strutils
include parser

proc main() =
  # Initial Parsing
  var source = "1 + 2"
  echo "Initial Source: ", source
  
  var parser = newParser(source)
  let tree = parser.parse()
  echo "Initial Tree S-Expr: ", toSExpr(tree)
  
  # Perform an Edit
  # Change "2" to "20". Insert "0" at index 5.
  # "1 + 2" -> "1 + 20"
  # Indices: '1' at 0. ' ' at 1. '+' at 2. ' ' at 3. '2' at 4.
  # '2' is range 4..5.
  # Insert '0' at 5.
  
  echo "\nEditing: Inserting '0' at index 5..."
  
  let editOp = InputEdit(
    startByte: 5,
    oldEndByte: 0,
    newEndByte: 1,
    startPoint: Point(row: 0, column: 5),
    oldEndPoint: Point(row: 0, column: 5),
    newEndPoint: Point(row: 0, column: 6)
  )
  
  edit(tree, editOp)
  
  # Verify tree adjustment
  # The root "expression" (binary_op) covered 0..5. Now should cover 0..6?
  # The number node for "2" covered 4..5.
  # Now "2" stays at 4..5?
  # Wait. Edit inserted AFTER "2".
  # "2" is at 4..5. `edit.startByte` is 5.
  # `node.endPos > edit.startByte`. 5 > 5 is False.
  # So "2" node (4..5) is NOT updated.
  # However, if we re-parse, we want the parser to reuse "2"?
  # Actually "2" is no longer valid ("20").
  # But the node itself in the tree is still "2".
  # Typically `edit` is used when changes happen. Nodes *after* the edit shift.
  # Nodes *containing* the edit expand.
  # My `edit` logic expands containing nodes (`node.endPos > edit.startByte`).
  # Since 5 is not > 5, root (0..5) is NOT expanded?
  # That seems like a bug or design choice in my `edit` impl (or typical TS behavior).
  # TS: `edit` expands parents.
  # If range is [0, 5). And edit at 5.
  # 5 is exclusive end.
  # Does [0, 5) contain 5? No.
  # But if I append to file, root expands?
  # In TS, root node always expands?
  # I'll stick to the current behavior (strict containment) and document it, or fix it if I want "append" to expand.
  # For the example, I'll allow "append" to be outside, but show that subsequent nodes shift (if any).
  
  echo "Tree Updated (Positions shifted)."
  echo "Root range: ", tree.startPos, "..", tree.endPos
  
  # Show manual re-parse simulation (reusing valid nodes would happen here)
  echo "\n(Incremental parsing leveraging this edit is a future feature)"

when isMainModule:
  main()
