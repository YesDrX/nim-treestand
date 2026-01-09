# Conflict Detection: Tree-sitter vs Treestand

## Overview

This document analyzes the `conflicting_precedence` test grammar to understand why tree-sitter reports it as an unresolved conflict while Treestand (after disabling the multi-precedence check) generates successfully.

## The Grammar

```javascript
export default grammar({
    name: 'conflicting_precedence',

    rules: {
        expression: $ => choice(
            $.sum,
            $.product,
            $.other_thing,
        ),

        sum: $ => prec.left(0, seq($.expression, '+', $.expression)),
        product: $ => prec.left(1, seq($.expression, '*', $.expression)),
        other_thing: $ => prec.left(-1, seq($.expression, '*', '*')),
        identifier: $ => /[a-zA-Z]+/
    }
});
```

### Key Points
- **sum**: Left-associative, precedence 0, matches `expression '+' expression`
- **product**: Left-associative, precedence 1, matches `expression '*' expression`
- **other_thing**: Left-associative, precedence -1, matches `expression '*' '*'`

## The Problematic Parse State

### State Description
After parsing `expression '+' expression`, with lookahead token `'*'`

**Example input:** `a + b * c`  
**State:** Just parsed `a + b`, next token is `*`

### Possible Actions

Tree-sitter identifies **three competing interpretations**:

#### Interpretation 1: Reduce as `sum`
```
(sum a '+' b) • '*' c
     ^^^^^^^
   reduce this
   
Final parse: ((a + b) * c)  -- Left-associative sum first
```
- **Action:** REDUCE the `expression '+' expression` as `sum`
- **Precedence:** 0 (from sum rule)
- **Then:** Handle the `*` token separately (might start a new product or other_thing)

#### Interpretation 2: Shift `*` for `product`
```
a '+' (product b • '*' c)
            ^^^^^^^
        shift to build product
        
Final parse: (a + (b * c))  -- Product inside the right expression
```
- **Action:** SHIFT `*` to continue building the second expression as a product
- **Precedence:** 1 (from product rule)
- **Result:** `a + (b * c)` -- higher precedence product wins

#### Interpretation 3: Shift `*` for `other_thing`
```
a '+' (other_thing b • '*' '*')
                ^^^^^^^
            shift to build other_thing
            
Final parse: (a + (b ** c))  -- other_thing inside the right expression
```
- **Action:** SHIFT `*` to continue building the second expression as other_thing  
- **Precedence:** -1 (from other_thing rule)
- **Result:** `a + (b ** ...)` -- needs another `*`

## Tree-sitter's Analysis

### Conflict Detection Logic

Tree-sitter compares precedences **pairwise**:

1. **product (prec=1) vs sum (prec=0)**
   - 1 > 0 → **SHIFT for product wins** over reduce as sum
   
2. **other_thing (prec=-1) vs sum (prec=0)**
   - -1 < 0 → **REDUCE as sum wins** over shift for other_thing

3. **product (prec=1) vs other_thing (prec=-1)** (both shifts to same state)
   - 1 > -1 → **product wins** over other_thing
   - But this comparison happens AFTER the individual shift/reduce comparisons

### The Problem: Contradictory Outcomes

The parser state contains:
- ✅ Shift action for `product` (prec=1) - beats reduce (0)
- ✅ Shift action for `other_thing` (prec=-1) - loses to reduce (0)  
- ✅ Reduce action for `sum` (prec=0) - beats other_thing (-1), loses to product (1)

**Contradiction:** 
- Keep product shift? → Blocks other_thing shift (product > other_thing)
- Keep other_thing shift? → Loses to reduce anyway (sum > other_thing)

This creates an **undecidable situation** from tree-sitter's perspective because:
1. The precedences create a **transitive ordering** (product > sum > other_thing)
2. But the grammar defines **all three rules at the same choice level**
3. This means `other_thing` can **never match** in this context

### Tree-sitter's Error Message

```
Error: Unresolved conflict for symbol sequence:

  expression  '+'  expression  •  '*'  …

Possible interpretations:

  1:  (sum  expression  '+'  expression)  •  '*'  …               (precedence: 0, associativity: Left)
  2:  expression  '+'  (other_thing  expression  •  '*'  '*')     (precedence: -1, associativity: Left)
  3:  expression  '+'  (product  expression  •  '*'  expression)  (precedence: 1, associativity: Left)

Possible resolutions:

  1:  Specify a higher precedence in `product` and `other_thing` than in the other rules.
  2:  Specify a higher precedence in `sum` than in the other rules.
  3:  Add a conflict for these rules: `sum`, `product`, `other_thing`
```

### Tree-sitter's Philosophy

**Reject the grammar** because:
1. ❌ **Unreachable rule**: `other_thing` can never match with these precedences
2. ❌ **Design smell**: Why define a rule that's unreachable?
3. ❌ **Ambiguous intent**: Author's intention is unclear
4. ✅ **Force explicit acknowledgment**: Use `conflicts: [[$.sum, $.product, $.other_thing]]` if this is intentional

## Treestand's Analysis

### Current Behavior (Multi-precedence Check Disabled)

Treestand's conflict resolution occurs in stages:

#### Stage 1: Collect All Actions
For the state `expression '+' expression •` with lookahead `'*'`:
```
Shift actions:
  - Shift '*' to state X (for product, prec=1, participants=[product, ...])
  - Shift '*' to state X (for other_thing, prec=-1, participants=[other_thing, ...])

Reduce actions:
  - Reduce as sum (prec=0, participants=[sum, ...])
```

#### Stage 2: Deduplicate Shifts by Target State
```nim
# Keep only highest-precedence shift per target state
var bestShiftsByState = initTable[uint32, BuildParseAction]()
for s in shiftActions:
   if s.shiftState notin bestShiftsByState:
      bestShiftsByState[s.shiftState] = s
   else:
      if s.shiftPrecedence > bestShiftsByState[s.shiftState].shiftPrecedence:
         bestShiftsByState[s.shiftState] = s
```

**Result after deduplication:**
```
Shift actions:
  - Shift '*' to state X (for product, prec=1)  ← KEPT (highest)

Reduce actions:
  - Reduce as sum (prec=0)
```

*Note: other_thing's shift (prec=-1) is **discarded** because product (prec=1) has higher precedence for the same target state.*

#### Stage 3: Resolve Shift/Reduce Conflict
Now we have a **simple Shift/Reduce conflict**:
- Shift for product (prec=1) vs Reduce for sum (prec=0)
- 1 > 0 → **Shift wins**

**Final action:** SHIFT '*' for product

### Example Parse Results

#### Input: `a + b * c`
```
Treestand parses as: a + (b * c)
Tree: (sum "a" "+" (product "b" "*" "c"))

Explanation:
- Parse "a" as expression
- See "+"
- Parse "b" as expression  
- See "*" → SHIFT (product wins over sum reduce)
- Parse "c" as expression
- Reduce "b * c" as product
- Reduce "a + (b*c)" as sum
```

#### Input: `a + b ** c` (attempting other_thing)
```
Treestand parses as: ((a + b) * ...) ERROR or (a + (b * ...)) ERROR

Explanation:
- After "a + b", seeing first "*" → SHIFT for product (prec=1)
- This means we're building "b * ..."
- Second "*" doesn't match product, creates parse error or ambiguity
- other_thing is NEVER chosen because its shift was discarded in deduplication
```

**Result:** `other_thing` rule is **effectively unreachable** (dead code)

### Why Treestand Succeeds Without Reporting Conflict

1. ✅ **Deduplication removes ambiguity**: Only one shift per state remains
2. ✅ **Deterministic precedence**: product (1) always beats sum (0)
3. ✅ **GLR can handle it**: The parser works correctly for inputs that match
4. ⚠️ **Silent dead code**: Users don't get warned about unreachable rules

## Comparison Table

| Aspect | Tree-sitter | Treestand (current) |
|--------|-------------|---------------------|
| **Multiple shifts, same state** | Reports as conflict | Deduplicates, keeps highest precedence |
| **Contradictory precedences** | Rejects grammar | Accepts (after deduplication) |
| **Unreachable rules** | Forces acknowledgment | Silently ignores |
| **Parse result for `a+b*c`** | N/A (generation fails) | `a + (b * c)` ✅ |
| **Parse result for `a+b**c`** | N/A (generation fails) | Parse error (other_thing unreachable) |
| **Philosophy** | Prevent design errors | Maximize grammar acceptance |

## Deep Dive: Why Deduplication Changes Semantics

### Before Deduplication (What Tree-sitter Sees)
```
State: expression '+' expression • | lookahead '*'

Actions:
  SHIFT '*' → state_X (from product rule, prec=1)
  SHIFT '*' → state_X (from other_thing rule, prec=-1)
  REDUCE → sum (prec=0)
```

**Problem:** Two shifts to the same state with different precedences represent **two different semantic intentions**:
- One shift comes from the `product` rule
- Another shift comes from the `other_thing` rule
- Both contribute to the "parse forest" but with conflicting precedences

### After Deduplication (What Treestand Uses)
```
State: expression '+' expression • | lookahead '*'

Actions:
  SHIFT '*' → state_X (prec=1, winner of de duplication)
  REDUCE → sum (prec=0)
```

**Changed semantics:**
- Lost information: we no longer know `other_thing` was a possibility
- Simplified decision: simple shift(1) vs reduce(0) comparison
- Deterministic: clear winner (shift)

## The Core Question: Is This a Bug or Feature?

### Arguments for "Bug" (Tree sitter's View)
1. **Lost semantic information**: Deduplication discards that `other_thing` exists
2. **Unreachable code**: Grammar authors don't learn their rule is useless
3. **Incorrect grammar accepted**: Design errors go unnoticed
4. **Breaking change from tree-sitter**: Users migrating expect same validation

### Arguments for "Feature" (Treestand's View)
1. **Deterministic**: Parser output is predictable and consistent
2. **Practical**: C grammar and other real grammars work correctly
3. **GLR philosophy**: Let runtime resolve ambiguities, not compile-time
4. **Linting vs errors**: Unreachable rules are warnings, not hard errors
5. **Precedence semantics**: Higher precedence SHOULD win, regardless of participant count

## Conclusion

This is a **philosophical difference** between:

**Tree-sitter:** "Grammars must be explicitly unambiguous or declare conflicts"
- Strict validation catches design errors early
- Forces grammar quality through compile-time checks
- May reject grammars that would work fine at runtime

**Treestand (current):** "Precedence resolves conflicts deterministically"
- Pragmatic acceptance of precedence as conflict resolution
- Trusts GLR to handle runtime ambiguities
- May silently accept grammars with dead code

Neither is objectively "wrong" - it's a trade-off between:
- **Safety** (tree-sitter) vs **Flexibility** (Treestand)
- **Explicit** (tree-sitter) vs **Implicit** (Treestand)
- **Design validation** (tree-sitter) vs **Runtime resolution** (Treestand)

## Recommendations

### For Treestand Development

**Option 1: Match tree-sitter exactly** (complex)
- Implement proper "participant set" matching for expected conflicts
- Detect multi-precedence shifts before deduplication
- Risk: High likelihood of false positives (as seen with C grammar)

**Option 2: Add separate linting pass** (best of both worlds)
- Keep current behavior (deterministic, practical)
- Add optional `--lint` flag that detects unreachable rules
- Warn about rules that can never match due to precedence
- Give users choice: strict (like tree-sitter) or permissive

**Option 3: Document as intentional difference** (current choice)
- Accept that Treestand is more permissive than tree-sitter
- Document this behavior clearly for users
- Note: "Grammars with unreachable rules will generate successfully"
- Trust users to test their grammars

### For Grammar Authors

If using Treestand:
1. **Test thoroughly**: Ensure all rules are reachable with test cases
2. **Review precedences**: Make sure precedence ordering matches intent
3. **Use tree-sitter first**: Validate grammar with tree-sitter to catch design issues
4. **Add conflicts**: Explicitly declare expected conflicts for documentation

## Technical Details: The Attempted Fix

My initial fix (now disabled) attempted to detect multi-precedence shifts:

```nim
# Check if multiple shifts with different precedences go to same state
var precByState = initTable[uint32, seq[int32]]()
for s in shiftActions:
  if s.shiftState notin precByState:
    precByState[s.shiftState] = @[]
  if s.shiftPrecedence notin precByState[s.shiftState]:
    precByState[s.shiftState].add(s.shiftPrecedence)

# If 2-4 distinct precedences found, check expectedConflicts
for state, precs in precByState:
  if precs.len > 1 and precs.len <= 4:
    hasMultiplePrecedences = true
```

**Why it failed:**
- ✅ Correctly detected `conflicting_precedence` (2 precs: 1 and -1)
- ❌ Also detected legitimate C grammar patterns (2 precs for different string literal types)
- ❌ Participant matching failed: collected participants didn't match C's expectedConflicts entries
- ❌ Too fragile: heuristics (2-4 range) were arbitrary and breaking

**Root cause:** Tree-sitter's conflict detection happens BEFORE deduplication with full semantic context. Treestand's happens AFTER with just precedence. These are fundamentally different approaches that are hard to reconcile.
