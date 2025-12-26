import std/options
import grammar


## DSL Helpers to build Grammar Rules easily
## 
## This module provides concise procedures to construct the AST of your grammar.
## Usage:
##   import treestand/dsl
##   rule: seq(sym("A"), choice(str("b"), patt("\d+")))

proc str*(s: string): Rule = 
  ## Matches a literal string.
  ## Example: `str("if")` matches the keyword "if".
  Rule(kind: rkString, stringValue: s)

proc patt*(p: string): Rule = 
  ## Matches a Regular Expression pattern.
  ## Example: `patt("\\w+")` matches identifiers.
  Rule(kind: rkPattern, patternValue: p)

proc sym*(name: string): Rule = 
  ## References another named rule (Non-Terminal) in the grammar.
  ## Example: `sym("expression")` matches the "expression" rule.
  Rule(kind: rkNamedSymbol, symbolName: name)

proc seq*(rules: varargs[Rule]): Rule = 
  ## Matches a sequence of rules in order.
  ## Example: `seq(str("func"), sym("block"))`
  Rule(kind: rkSeq, seqMembers: @rules)

proc choice*(rules: varargs[Rule]): Rule = 
  ## Matches ONE of the provided rules (Ordered Choice / Alternation).
  ## Example: `choice(str("true"), str("false"))`
  Rule(kind: rkChoice, choiceMembers: @rules)

proc rep*(r: Rule): Rule = 
  ## Matches one or more occurrences of the rule (Repeat).
  ## Example: `rep(sym("digit"))` matches one or more digits.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkRepeat, repeatContent: c)

proc opt*(r: Rule): Rule =
  ## Matches zero or one occurrence of the rule (Optional).
  ## Effectively `choice(r, blank)`.
  Rule(kind: rkChoice, choiceMembers: @[r, Rule(kind: rkBlank)])

proc token*(r: Rule): Rule =
  ## Marks the rule as a Lexical Token.
  ## The parser treats tokens as atomic units; their internal structure is handled by the Lexer.
  ## Use this for things like identifiers, numbers, or keywords that shouldn't be parsed recursively by the parser logic.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata, metadataParams: MetadataParams(isToken: true), metadataRule: c)

proc prec*(p: int, r: Rule): Rule =
  ## Assigns a static precedence level to the rule.
  ## Higher values bind tighter.
  ## Used to resolve shift/reduce conflicts (e.g. operator precedence).
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata, 
       metadataParams: MetadataParams(
         precedence: newPrecedence(p.int32)
       ), 
       metadataRule: c)

proc prec_left*(p: int, r: Rule): Rule =
  ## Assigns Left Associativity and precedence.
  ## Example: `1 + 2 + 3` -> `(1 + 2) + 3`.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata, 
       metadataParams: MetadataParams(
         precedence: newPrecedence(p.int32), 
         associativity: some(gaLeft)
       ), 
       metadataRule: c)

proc prec_right*(p: int, r: Rule): Rule =
  ## Assigns Right Associativity and precedence.
  ## Example: `a = b = c` -> `a = (b = c)`.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata, 
       metadataParams: MetadataParams(
         precedence: newPrecedence(p.int32), 
         associativity: some(gaRight)
       ), 
       metadataRule: c)

proc prec_dynamic*(p: int, r: Rule): Rule =
  ## Assigns Dynamic Precedence.
  ## Helps resolve GLR conflicts at runtime by favoring paths with higher dynamic precedence.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata, 
       metadataParams: MetadataParams(
         dynamicPrecedence: p.int32
       ), 
       metadataRule: c)

proc field*(name: string, r: Rule): Rule =
  ## Assigns a field name to the rule.
  ## Used for generating named fields in the resulting CST/AST nodes.
  var c = new(Rule)
  c[] = r
  Rule(kind: rkMetadata,
       metadataParams: MetadataParams(
         fieldName: some(name)
       ),
       metadataRule: c)
