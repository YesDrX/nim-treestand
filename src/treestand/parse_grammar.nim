## Parse grammar JSON into InputGrammar structure

import json, grammar, rules, sets, options, os
import js_exec

type
  ParseGrammarError* = object of CatchableError
  GrammarJson* = object
    name*: string
    rules*: JsonNode
    precedences*: seq[seq[RuleJson]]
    conflicts*: seq[seq[string]]
    externals*: seq[RuleJson]
    extras*: seq[RuleJson]
    inline*: seq[string]
    supertypes*: seq[string]
    word*: Option[string]
    reserved*: JsonNode

  RuleJsonKind* = enum
    rjkAlias, rjkBlank, rjkString, rjkPattern, rjkSymbol, rjkChoice,
    rjkField, rjkSeq, rjkRepeat, rjkRepeat1, rjkPrecDynamic,
    rjkPrecLeft, rjkPrecRight, rjkPrec, rjkToken, rjkImmediateToken, rjkReserved

  PrecedenceValueJson* = object
    case isName*: bool
    of true:
      name*: string
    of false:
      intValue*: int32

  RuleJson* = object
    case kind*: RuleJsonKind
    of rjkAlias:
      aliasContent*: ref RuleJson
      aliasNamed*: bool
      aliasValue*: string
    of rjkBlank:
      discard
    of rjkString:
      stringValue*: string
    of rjkPattern:
      patternValue*: string
      patternFlags*: Option[string]
    of rjkSymbol:
      symbolName*: string
    of rjkChoice:
      choiceMembers*: seq[RuleJson]
    of rjkField:
      fieldName*: string
      fieldContent*: ref RuleJson
    of rjkSeq:
      seqMembers*: seq[RuleJson]
    of rjkRepeat:
      repeatContent*: ref RuleJson
    of rjkRepeat1:
      repeat1Content*: ref RuleJson
    of rjkPrecDynamic:
      precDynamicValue*: int32
      precDynamicContent*: ref RuleJson
    of rjkPrecLeft:
      precLeftValue*: PrecedenceValueJson
      precLeftContent*: ref RuleJson
    of rjkPrecRight:
      precRightValue*: PrecedenceValueJson
      precRightContent*: ref RuleJson
    of rjkPrec:
      precValue*: PrecedenceValueJson
      precContent*: ref RuleJson
    of rjkToken:
      tokenContent*: ref RuleJson
    of rjkImmediateToken:
      immediateTokenContent*: ref RuleJson
    of rjkReserved:
      reservedContextName*: string
      reservedContent*: ref RuleJson
    case isName*: bool
    of true:
      name*: string
    of false:
      intValue*: int32

proc parseRuleJson*(node: JsonNode): RuleJson {.raises: [ParseGrammarError].} =
  let typ = node{"type"}.getStr()
  case typ
  of "ALIAS":
    var aliasContent = new(RuleJson)
    aliasContent[] = parseRuleJson(node{"content"})
    RuleJson(
      kind: rjkAlias,
      aliasContent: aliasContent,
      aliasNamed: node{"named"}.getBool(),
      aliasValue: node{"value"}.getStr()
    )
  of "BLANK":
    RuleJson(kind: rjkBlank)
  of "STRING":
    RuleJson(kind: rjkString, stringValue: node{"value"}.getStr())
  of "PATTERN":
    RuleJson(
      kind: rjkPattern,
      patternValue: node{"value"}.getStr(),
      patternFlags: if node.hasKey("flags"): some(node{"flags"}.getStr()) else: none(string)
    )
  of "SYMBOL":
    RuleJson(kind: rjkSymbol, symbolName: node{"name"}.getStr())
  of "CHOICE":
    var members: seq[RuleJson] = @[]
    for m in node{"members"}:
      members.add(parseRuleJson(m))
    RuleJson(kind: rjkChoice, choiceMembers: members)
  of "FIELD":
    RuleJson(
      kind: rjkField,
      fieldName: node{"name"}.getStr(),
      fieldContent: (block:
        var fc = new(RuleJson)
        fc[] = parseRuleJson(node{"content"})
        fc
      )
    )
  of "SEQ":
    var members: seq[RuleJson] = @[]
    for m in node{"members"}:
      members.add(parseRuleJson(m))
    RuleJson(kind: rjkSeq, seqMembers: members)
  of "REPEAT":
    RuleJson(
      kind: rjkRepeat,
      repeatContent: (block:
        var rc = new(RuleJson)
        rc[] = parseRuleJson(node{"content"})
        rc
      )
    )
  of "REPEAT1":
    var repeat1Content = new(RuleJson)
    repeat1Content[] = parseRuleJson(node{"content"})
    RuleJson(
      kind: rjkRepeat1,
      repeat1Content: repeat1Content
    )
  of "PREC_DYNAMIC":
    let val = node{"value"}.getInt().int32
    RuleJson(
      kind: rjkPrecDynamic,
      precDynamicValue: val,
      precDynamicContent: (block:
        var pdc = new(RuleJson)
        pdc[] = parseRuleJson(node{"content"})
        pdc
      )
    )
  of "PREC_LEFT", "PREC_RIGHT", "PREC":
    let valNode = node{"value"}
    var precVal: PrecedenceValueJson
    if valNode.kind == JString:
      precVal = PrecedenceValueJson(isName: true, name: valNode.getStr())
    else:
      precVal = PrecedenceValueJson(isName: false, intValue: valNode.getInt().int32)
    
    case typ
    of "PREC_LEFT":
      RuleJson(
        kind: rjkPrecLeft,
        precLeftValue: precVal,
        precLeftContent: (block:
        var plc = new(RuleJson)
        plc[] = parseRuleJson(node{"content"})
        plc
      )
      )
    of "PREC_RIGHT":
      RuleJson(
        kind: rjkPrecRight,
        precRightValue: precVal,
        precRightContent: (block:
        var prc = new(RuleJson)
        prc[] = parseRuleJson(node{"content"})
        prc
      )
      )
    else:
      RuleJson(
        kind: rjkPrec,
        precValue: precVal,
        precContent: (block:
        var pc = new(RuleJson)
        pc[] = parseRuleJson(node{"content"})
        pc
      )
      )
  of "TOKEN":
    RuleJson(
      kind: rjkToken,
      tokenContent: (block:
        var tc = new(RuleJson)
        tc[] = parseRuleJson(node{"content"})
        tc
      )
    )
  of "IMMEDIATE_TOKEN":
    RuleJson(
      kind: rjkImmediateToken,
      immediateTokenContent: (block:
        var itc = new(RuleJson)
        itc[] = parseRuleJson(node{"content"})
        itc
      )
    )
  of "RESERVED":
    RuleJson(
      kind: rjkReserved,
      reservedContextName: node{"context_name"}.getStr(),
      reservedContent: (block:
        var rc = new(RuleJson)
        rc[] = parseRuleJson(node{"content"})
        rc
      )
    )
  else:
    raise newException(ParseGrammarError, "Unknown rule type: " & typ)

proc parseRule(rj: RuleJson, isExternal: bool): Rule =
  case rj.kind
  of rjkBlank:
    Rule(kind: rkBlank)
  of rjkString:
    Rule(kind: rkString, stringValue: rj.stringValue)
  of rjkPattern:
    Rule(
      kind: rkPattern,
      patternValue: rj.patternValue,
      patternFlags: rj.patternFlags.get("")
    )
  of rjkSymbol:
    Rule(kind: rkNamedSymbol, symbolName: rj.symbolName)
  of rjkChoice:
    var members: seq[Rule] = @[]
    for m in rj.choiceMembers:
      members.add(parseRule(m, isExternal))
    Rule(kind: rkChoice, choiceMembers: members)
  of rjkField:
    var content = parseRule(rj.fieldContent[], isExternal)
    Rule(
      kind: rkMetadata,
      metadataParams: MetadataParams(fieldName: some(rj.fieldName)),
      metadataRule: (block:
        var mr = new(Rule)
        mr[] = content
        mr
      )
    )
  of rjkSeq:
    var members: seq[Rule] = @[]
    for m in rj.seqMembers:
      members.add(parseRule(m, isExternal))
    Rule(kind: rkSeq, seqMembers: members)
  of rjkRepeat:
    var content = parseRule(rj.repeatContent[], isExternal)
    var repeatContent = new(Rule)
    repeatContent[] = content
    Rule(kind: rkRepeat, repeatContent: repeatContent)
  of rjkRepeat1:
    # REPEAT1 is equivalent to seq(content, repeat(content))
    var content = parseRule(rj.repeat1Content[], isExternal)
    var repeatContent = new(Rule)
    repeatContent[] = content
    var repeatRule = Rule(kind: rkRepeat, repeatContent: repeatContent)
    Rule(kind: rkSeq, seqMembers: @[content, repeatRule])
  of rjkPrecDynamic:
    var content = parseRule(rj.precDynamicContent[], isExternal)
    var params = MetadataParams(dynamicPrecedence: rj.precDynamicValue)
    var metadataRule = new(Rule)
    metadataRule[] = content
    Rule(kind: rkMetadata, metadataParams: params, metadataRule: metadataRule)
  of rjkPrecLeft, rjkPrecRight, rjkPrec:
    var content: Rule
    var prec: Precedence
    var assoc: Option[GrammarAssociativity]
    
    case rj.kind
    of rjkPrecLeft:
      content = parseRule(rj.precLeftContent[], isExternal)
      if rj.precLeftValue.isName:
        prec = newPrecedence(rj.precLeftValue.name)
      else:
        prec = newPrecedence(rj.precLeftValue.intValue)
      assoc = some(gaLeft)
    of rjkPrecRight:
      content = parseRule(rj.precRightContent[], isExternal)
      if rj.precRightValue.isName:
        prec = newPrecedence(rj.precRightValue.name)
      else:
        prec = newPrecedence(rj.precRightValue.intValue)
      assoc = some(gaRight)
    else:
      content = parseRule(rj.precContent[], isExternal)
      if rj.precValue.isName:
        prec = newPrecedence(rj.precValue.name)
      else:
        prec = newPrecedence(rj.precValue.intValue)
      assoc = none(GrammarAssociativity)
    
    var params = MetadataParams(precedence: prec, associativity: assoc)
    var metadataRule = new(Rule)
    metadataRule[] = content
    Rule(kind: rkMetadata, metadataParams: params, metadataRule: metadataRule)
  of rjkToken:
    var content = parseRule(rj.tokenContent[], isExternal)
    var params = MetadataParams(isToken: true)
    var metadataRule = new(Rule)
    metadataRule[] = content
    Rule(kind: rkMetadata, metadataParams: params, metadataRule: metadataRule)
  of rjkImmediateToken:
    var content = parseRule(rj.immediateTokenContent[], isExternal)
    var params = MetadataParams(isToken: true, isMainToken: true)
    var metadataRule = new(Rule)
    metadataRule[] = content
    Rule(kind: rkMetadata, metadataParams: params, metadataRule: metadataRule)
  of rjkReserved:
    var content = parseRule(rj.reservedContent[], isExternal)
    Rule(
      kind: rkReserved,
      reservedRule: (block:
        var rr = new(Rule)
        rr[] = content
        rr
      ),
      reservedContextName: rj.reservedContextName
    )
  of rjkAlias:
    var content = parseRule(rj.aliasContent[], isExternal)
    var alias = Alias(value: rj.aliasValue, isNamed: rj.aliasNamed)
    var params = MetadataParams(alias: some(alias))
    var metadataRule = new(Rule)
    metadataRule[] = content
    Rule(kind: rkMetadata, metadataParams: params, metadataRule: metadataRule)

proc ruleIsReferenced(rule: Rule, target: string, isExternal: bool): bool =
  case rule.kind
  of rkNamedSymbol:
    rule.symbolName == target and not isExternal
  of rkChoice, rkSeq:
    for r in (if rule.kind == rkChoice: rule.choiceMembers else: rule.seqMembers):
      if ruleIsReferenced(r, target, false):
        return true
    false
  of rkMetadata, rkReserved:
    let inner = if rule.kind == rkMetadata: rule.metadataRule[] else: rule.reservedRule[]
    ruleIsReferenced(inner, target, isExternal)
  of rkRepeat:
    ruleIsReferenced(rule.repeatContent[], target, false)
  else:
    false

proc variableIsUsed(
  grammarRules: seq[(string, Rule)],
  extras: seq[Rule],
  externals: seq[Rule],
  targetName: string,
  inProgress: var HashSet[string]
): bool =
  if grammarRules.len == 0:
    return false
  let root = grammarRules[0][0]
  if targetName == root:
    return true
  
  for extra in extras:
    if ruleIsReferenced(extra, targetName, false):
      return true
  
  for external in externals:
    if ruleIsReferenced(external, targetName, true):
      return true
  
  inProgress.incl(targetName)
  result = false
  for (name, rule) in grammarRules:
    if name != targetName:
      if ruleIsReferenced(rule, targetName, false) and name notin inProgress:
        if variableIsUsed(grammarRules, extras, externals, name, inProgress):
          result = true
          break
  inProgress.excl(targetName)
  return result

proc parseGrammar*(input: string): InputGrammar {.raises: [ParseGrammarError, JsonParsingError, IOError, OSError, ValueError].} =
  ## Parse a Tree-sitter grammar from JSON format into an `InputGrammar` object.
  ##
  ## This is the core grammar parsing procedure that converts Tree-sitter grammar JSON
  ## (typically generated from `grammar.js` files) into Treestand's internal representation.
  ## The parsed grammar can then be processed through `prepareGrammar` and `buildTables`
  ## to generate a complete parser.
  ##
  ## **Parameters:**
  ## - `input`: A JSON string containing the grammar definition with the following structure:
  ##   - `name`: Grammar name
  ##   - `rules`: Map of rule names to rule definitions
  ##   - `extras`: List of tokens to skip (e.g., whitespace, comments)
  ##   - `externals`: List of external scanner tokens
  ##   - `conflicts`: List of intentional conflicts to resolve
  ##   - `precedences`: List of precedence declarations
  ##   - `inline`: List of rules to inline during parsing
  ##   - `supertypes`: List of supertype symbol names
  ##   - `word`: Optional identifier for keyword recognition
  ##
  ## **Returns:**
  ## An `InputGrammar` object containing all parsed rules, metadata, and configuration.
  ##
  ## **Raises:**
  ## - `ParseGrammarError`: If the grammar structure is invalid or contains errors
  ## - `JsonParsingError`: If the input is not valid JSON
  ## - `IOError`: On I/O errors during processing
  ## - `OSError`: On operating system errors
  ## - `ValueError`: If values are out of expected range or format
  ##
  ## **Example:**
  ## ```nim
  ## let jsonStr = """{
  ##   "name": "simple",
  ##   "rules": {"expression": {"type": "STRING", "value": "hello"}},
  ##   "extras": [],
  ##   "externals": []
  ## }"""
  ## let grammar = parseGrammar(jsonStr)
  ## echo grammar.name  # "simple"
  ## ```
  let jsonNode = parseJson(input)
  
  var grammarJson: GrammarJson
  grammarJson.name = jsonNode{"name"}.getStr()
  
  # Parse rules
  var rules: seq[(string, Rule)] = @[]
  for key, val in jsonNode{"rules"}:
    let ruleJson = parseRuleJson(val)
    rules.add((key, parseRule(ruleJson, false)))
  
  # Parse extras
  var extraSymbols: seq[Rule] = @[]
  for extra in jsonNode{"extras"}:
    let ruleJson = parseRuleJson(extra)
    let rule = parseRule(ruleJson, false)
    if rule.kind == rkString and rule.stringValue.len == 0:
      raise newException(ParseGrammarError, "Rules in extras array must not contain empty strings")
    extraSymbols.add(rule)
  
  # Parse externals
  var externalTokens: seq[Rule] = @[]
  for external in jsonNode{"externals"}:
    let ruleJson = parseRuleJson(external)
    externalTokens.add(parseRule(ruleJson, false))
  
  # Parse precedences
  var precedenceOrderings: seq[seq[PrecedenceEntry]] = @[]
  for precList in jsonNode{"precedences"}:
    var ordering: seq[PrecedenceEntry] = @[]
    for entry in precList:
      let ruleJson = parseRuleJson(entry)
      case ruleJson.kind
      of rjkString:
        ordering.add(PrecedenceEntry(isName: true, name: ruleJson.stringValue))
      of rjkSymbol:
        ordering.add(PrecedenceEntry(isName: false, symbol: ruleJson.symbolName))
      else:
        raise newException(ParseGrammarError, "Invalid rule in precedences array")
    precedenceOrderings.add(ordering)
  
  # Parse conflicts
  var expectedConflicts: seq[seq[string]] = @[]
  for conflict in jsonNode{"conflicts"}:
    var conflictList: seq[string] = @[]
    for name in conflict:
      conflictList.add(name.getStr())
    expectedConflicts.add(conflictList)
  
  # Filter unused variables
  var inProgress = initHashSet[string]()
  var variables: seq[Variable] = @[]
  let wordToken = if jsonNode.hasKey("word"): some(jsonNode{"word"}.getStr()) else: none(string)
  
  for (name, rule) in rules:
    if wordToken.isNone or wordToken.get() != name:
      if not variableIsUsed(rules, extraSymbols, externalTokens, name, inProgress):
        continue
    
    variables.add(Variable(
      name: name,
      kind: vtNamed,
      rule: rule
    ))
  
  # Parse reserved words
  var reservedWords: seq[ReservedWordContext[Rule]] = @[]
  if jsonNode.hasKey("reserved"):
    for key, val in jsonNode{"reserved"}:
      if val.kind != JArray:
        raise newException(ParseGrammarError, "Reserved word sets must be arrays")
      var reservedTokens: seq[Rule] = @[]
      for token in val:
        let ruleJson = parseRuleJson(token)
        reservedTokens.add(parseRule(ruleJson, false))
      reservedWords.add(ReservedWordContext[Rule](
        name: key,
        reservedWords: reservedTokens
      ))
  
  InputGrammar(
    name: grammarJson.name,
    variables: variables,
    extraSymbols: extraSymbols,
    expectedConflicts: expectedConflicts,
    precedenceOrderings: precedenceOrderings,
    externalTokens: externalTokens,
    variablesToInline: if jsonNode.hasKey("inline"): 
      (block:
        var rst: seq[string] = @[]
        for name in jsonNode{"inline"}:
          rst.add(name.getStr())
        rst
      ) else: @[],
    supertypeSymbols: if jsonNode.hasKey("supertypes"):
      (block:
        var rst: seq[string] = @[]
        for name in jsonNode{"supertypes"}:
          rst.add(name.getStr())
        rst
      ) else: @[],
    wordToken: wordToken,
    reservedWords: reservedWords
  )

proc parseGrammarJson*(jsonFilename: string): InputGrammar {.raises: [ParseGrammarError, JsonParsingError, IOError, OSError, ValueError].} =
  ## Parse a Tree-sitter grammar from a JSON file.
  ##
  ## This is a convenience wrapper around `parseGrammar` that reads grammar JSON
  ## from a file. This is commonly used when working with pre-generated grammar JSON
  ## files (e.g., from `tree-sitter generate` or custom scripts).
  ##
  ## **Parameters:**
  ## - `jsonFilename`: Path to the JSON file containing the grammar definition
  ##
  ## **Returns:**
  ## An `InputGrammar` object parsed from the file contents.
  ##
  ## **Raises:**
  ## - `IOError`: If the file doesn't exist or cannot be read
  ## - `ParseGrammarError`: If the grammar structure is invalid
  ## - `JsonParsingError`: If the file doesn't contain valid JSON
  ## - `OSError`: On operating system errors
  ## - `ValueError`: If values are out of expected range or format
  ##
  ## **Example:**
  ## ```nim
  ## let grammar = parseGrammarJson("path/to/grammar.json")
  ## echo grammar.name
  ## ```
  if not jsonFilename.fileExists():
    raise newException(IOError, "File not found: " & jsonFilename)
  return parseGrammar(readFile(jsonFilename))

proc parseGrammarJs*(jsFilename: string): InputGrammar {.raises: [ParseGrammarError, JsonParsingError, IOError, OSError, ValueError, JsExecError].} =
  ## Parse a Tree-sitter grammar from a `grammar.js` file.
  ##
  ## This procedure executes a `grammar.js` file (standard Tree-sitter format) using
  ## a JavaScript runtime (Node.js, Bun, or Deno) to generate the grammar JSON, then
  ## parses it into an `InputGrammar` object. This provides seamless integration with
  ## existing Tree-sitter grammar files.
  ##
  ## The procedure automatically:
  ## 1. Locates the `dsl.js` helper file (bundled with Treestand)
  ## 2. Finds an available JS runtime (checks for `bun`, `node`, then `deno`)
  ## 3. Executes the `grammar.js` file to generate JSON output
  ## 4. Parses the resulting JSON into an `InputGrammar`
  ##
  ## **Parameters:**
  ## - `jsFilename`: Path to the `grammar.js` file to execute
  ##
  ## **Returns:**
  ## An `InputGrammar` object generated from the `grammar.js` file.
  ##
  ## **Raises:**
  ## - `JsExecError`: If no JS runtime is found or execution fails
  ## - `IOError`: If the file doesn't exist or cannot be read
  ## - `ParseGrammarError`: If the generated grammar structure is invalid
  ## - `JsonParsingError`: If the JS output is not valid JSON
  ## - `OSError`: On operating system errors
  ## - `ValueError`: If values are out of expected range or format
  ##
  ## **Example:**
  ## ```nim
  ## # Parse a standard Tree-sitter grammar.js file
  ## let grammar = parseGrammarJs("grammars/tree-sitter-json/grammar.js")
  ## echo grammar.name  # "json"
  ## ```
  let jsonStr = executeGrammarJs(jsFilename)
  return parseGrammar(jsonStr)
