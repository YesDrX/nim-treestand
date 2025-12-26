# prepare_grammar.nim
## Grammar preparation - flatten, expand tokens, process inlines, etc.

import grammar, nfa, sets, options
from std/tables import contains, `[]`, `[]=`
import std/tables as stdtables
import std/strutils

type
  PrepareGrammarError* = object of CatchableError
  
  InternedGrammar* = object
    variables*: seq[Variable]
    extraSymbols*: seq[Rule]
    expectedConflicts*: seq[seq[GrammarSymbol]]
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    externalTokens*: seq[Variable]
    variablesToInline*: seq[GrammarSymbol]
    supertypeSymbols*: seq[GrammarSymbol]
    wordToken*: Option[GrammarSymbol]
    reservedWordSets*: seq[ReservedWordContext[Rule]]

proc variableTypeForName*(name: string): VariableType =
  ## Determine variable type from name (hidden if starts with _)
  if name.len > 0 and name[0] == '_':
    if name.len > 1 and name[1] == '_':
      vtAuxiliary
    else:
      vtHidden
  else:
    vtNamed

proc internSymbols*(input: InputGrammar): InternedGrammar =
  ## Convert named symbols to GrammarSymbol indices
  var nameToSymbol = stdtables.initTable[string, GrammarSymbol]()
  var stringToExternal = stdtables.initTable[string, GrammarSymbol]()
  var symbolIndex: uint16 = 0
  
  # Create symbols for all variables
  for variable in input.variables:
    let symbol = GrammarSymbol(kind: stNonTerminal, index: symbolIndex)
    nameToSymbol[variable.name] = symbol
    inc symbolIndex
  
  # Register external tokens in nameToSymbol or stringToExternal so they can be referenced
  # External tokens get stExternal kind
  var externalIndex: uint16 = 0
  for externalRule in input.externalTokens:
    let symbol = GrammarSymbol(kind: stExternal, index: externalIndex)
    if externalRule.kind == rkNamedSymbol:
      nameToSymbol[externalRule.symbolName] = symbol
    elif externalRule.kind == rkString:
      stringToExternal[externalRule.stringValue] = symbol
    inc externalIndex
  
  # Convert rules to use GrammarSymbol instead of NamedSymbol
  proc toRef(r: Rule): ref Rule =
    new(result)
    result[] = r

  proc internRule(rule: Rule, varName: Option[string]): Rule =
    case rule.kind
    of rkNamedSymbol:
      let symbol = nameToSymbol.getOrDefault(rule.symbolName)
      if symbol.kind == stNonTerminal or symbol.kind == stExternal:
        Rule(kind: rkSymbol, symbol: symbol)
      else:
        raise newException(PrepareGrammarError, "Undefined symbol: " & rule.symbolName)
    of rkString:
       if rule.stringValue in stringToExternal:
         Rule(kind: rkSymbol, symbol: stringToExternal[rule.stringValue])
       else:
         rule
    of rkChoice:
      var members: seq[Rule] = @[]
      for m in rule.choiceMembers:
        members.add(internRule(m, varName))
      Rule(kind: rkChoice, choiceMembers: members)
    of rkSeq:
      var members: seq[Rule] = @[]
      for m in rule.seqMembers:
        members.add(internRule(m, varName))
      Rule(kind: rkSeq, seqMembers: members)
    of rkMetadata:
      var inner = internRule(rule.metadataRule[], varName)
      Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))
    of rkRepeat:
      var inner = internRule(rule.repeatContent[], varName)
      Rule(kind: rkRepeat, repeatContent: toRef(inner))
    of rkReserved:
      var inner = internRule(rule.reservedRule[], varName)
      Rule(kind: rkReserved, reservedRule: toRef(inner), reservedContextName: rule.reservedContextName)
    else:
      rule
  
  # Convert variables
  var variables: seq[Variable] = @[]
  for variable in input.variables:
    variables.add(Variable(
      name: variable.name,
      kind: variableTypeForName(variable.name),
      rule: internRule(variable.rule, some(variable.name))
    ))
  
  # Convert external tokens
  var externalTokens: seq[Variable] = @[]
  for externalRule in input.externalTokens:
    let rule = internRule(externalRule, none(string))
    var name = ""
    case externalRule.kind
    of rkNamedSymbol: name = externalRule.symbolName
    of rkString: name = externalRule.stringValue
    of rkPattern: name = externalRule.patternValue
    else: discard
    
    externalTokens.add(Variable(
      name: name,
      kind: if name.len > 0: variableTypeForName(name) else: vtAnonymous,
      rule: rule
    ))
  
  # Convert extra symbols
  var extraSymbols: seq[Rule] = @[]
  for extra in input.extraSymbols:
    extraSymbols.add(internRule(extra, none(string)))
  
  # Convert supertype symbols
  var supertypeSymbols: seq[GrammarSymbol] = @[]
  for name in input.supertypeSymbols:
    if name in nameToSymbol:
      supertypeSymbols.add(nameToSymbol[name])
    else:
      raise newException(PrepareGrammarError, "Undefined supertype: " & name)
  
  # Convert conflicts
  var expectedConflicts: seq[seq[GrammarSymbol]] = @[]
  for conflict in input.expectedConflicts:
    var conflictSymbols: seq[GrammarSymbol] = @[]
    for name in conflict:
      if name in nameToSymbol:
        conflictSymbols.add(nameToSymbol[name])
      else:
        raise newException(PrepareGrammarError, "Undefined conflict symbol: " & name)
    expectedConflicts.add(conflictSymbols)
  
  # Convert variables to inline
  var variablesToInline: seq[GrammarSymbol] = @[]
  for name in input.variablesToInline:
    if name in nameToSymbol:
      variablesToInline.add(nameToSymbol[name])
  
  # Convert word token
  var wordToken: Option[GrammarSymbol] = none(GrammarSymbol)
  if input.wordToken.isSome:
    let name = input.wordToken.get()
    if name in nameToSymbol:
      wordToken = some(nameToSymbol[name])
    else:
      raise newException(PrepareGrammarError, "Undefined word token: " & name)
  
  # Convert reserved words
  var reservedWordSets: seq[ReservedWordContext[Rule]] = @[]
  for reservedSet in input.reservedWords:
    var internedRules: seq[Rule] = @[]
    for rule in reservedSet.reservedWords:
      internedRules.add(internRule(rule, none(string)))
    reservedWordSets.add(ReservedWordContext[Rule](
      name: reservedSet.name,
      reservedWords: internedRules
    ))
  
  # Check start rule is visible
  if variables.len > 0:
    if variableTypeForName(variables[0].name) == vtHidden:
      raise newException(PrepareGrammarError, "A grammar's start rule must be visible.")
  
  InternedGrammar(
    variables: variables,
    extraSymbols: extraSymbols,
    expectedConflicts: expectedConflicts,
    precedenceOrderings: input.precedenceOrderings,
    externalTokens: externalTokens,
    variablesToInline: variablesToInline,
    supertypeSymbols: supertypeSymbols,
    wordToken: wordToken,
    reservedWordSets: reservedWordSets
  )

proc extractChoices*(rule: Rule): seq[Rule] =
  ## Extract all choices from a rule (flatten CHOICE nodes)
  case rule.kind
  of rkChoice:
    var choices: seq[Rule] = @[]
    for member in rule.choiceMembers:
      let subChoices = extractChoices(member)
      choices.add(subChoices)
    return choices
  of rkSeq:
    # Cartesian product of choices in sequence members
    # e.g. Seq(A, Choice(B, C)) -> Choice(Seq(A, B), Seq(A, C))
    var memberOptions: seq[seq[Rule]] = @[]
    for m in rule.seqMembers:
      memberOptions.add(extractChoices(m))
      
    var results: seq[seq[Rule]] = @[ @[] ]
    for options in memberOptions:
      var nextResults: seq[seq[Rule]] = @[]
      for res in results:
        for opt in options:
          var newRes = res
          newRes.add(opt)
          nextResults.add(newRes)
      results = nextResults
      
    var choices: seq[Rule] = @[]
    for res in results:
      choices.add(Rule(kind: rkSeq, seqMembers: res))
    return choices
  of rkMetadata:
    # Extract choices from the wrapped rule and re-wrap each with metadata
    let innerChoices = extractChoices(rule.metadataRule[])
    result = @[]
    for innerChoice in innerChoices:
      var wrappedRule: ref Rule
      new(wrappedRule)
      wrappedRule[] = innerChoice
      result.add(Rule(
        kind: rkMetadata,
        metadataParams: rule.metadataParams,
        metadataRule: wrappedRule
      ))
    return result
  else:
    return @[rule]

proc isLexical(rule: Rule): bool =
  ## Check if a rule is implicitly lexical (String or Pattern)
  case rule.kind
  of rkString, rkPattern: true
  of rkMetadata: isLexical(rule.metadataRule[])
  of rkChoice:
    for m in rule.choiceMembers:
      if not isLexical(m): return false
    true
  of rkSeq:
    for m in rule.seqMembers:
      if not isLexical(m): return false
    true
  of rkRepeat: isLexical(rule.repeatContent[])
  else: false

proc extractTokens(interned: InternedGrammar): tuple[syntaxVars: seq[Variable], lexicalVars: seq[LexicalVariable], varToLex: stdtables.Table[int, GrammarSymbol]] =
  var syntaxVars: seq[Variable] = @[]
  var lexicalVars: seq[LexicalVariable] = @[]
  var lexicalMap = stdtables.initTable[Rule, GrammarSymbol]() 
  var nextLexicalIdx: uint16 = 0
  
  # Map variable indices to their lexical symbol if they are token-wrapped
  var variableToLexicalSymbol = stdtables.initTable[int, GrammarSymbol]()

  proc toRef(r: Rule): ref Rule =
    new(result)
    result[] = r

  # Helper to process a rule and replace lexical parts
  proc processRule(rule: Rule, varName: Option[string] = none(string)): Rule =
    if rule.kind == rkString or rule.kind == rkPattern:
      # This is a lexical token.
      # Check if we already have it
      if rule in lexicalMap:
        return Rule(kind: rkSymbol, symbol: lexicalMap[rule])
      
      # Create new lexical variable
      let name = case rule.kind
        of rkString: "'" & rule.stringValue & "'"
        of rkPattern: "/" & rule.patternValue & "/"
        else: "token_" & $nextLexicalIdx
      
      let sym = GrammarSymbol(kind: stTerminal, index: nextLexicalIdx)
      lexicalVars.add(LexicalVariable(
        name: name,
        kind: vtAnonymous,
        implicitPrecedence: 0,
        startState: 0, # To be set by build_tables
        rule: rule
      ))
      lexicalMap[rule] = sym
      inc nextLexicalIdx
      return Rule(kind: rkSymbol, symbol: sym)
      
    case rule.kind
    of rkChoice:
      var members: seq[Rule] = @[]
      for m in rule.choiceMembers:
        members.add(processRule(m, varName))
      Rule(kind: rkChoice, choiceMembers: members)
    of rkSeq:
      var members: seq[Rule] = @[]
      for m in rule.seqMembers:
        members.add(processRule(m, varName))
      Rule(kind: rkSeq, seqMembers: members)
    of rkMetadata:
      if rule.metadataParams.isToken:
        # Check if we already have it
        if rule in lexicalMap:
          return Rule(kind: rkSymbol, symbol: lexicalMap[rule])
          
        # Create new lexical variable for the entire token content
        let symIndex = nextLexicalIdx
        inc nextLexicalIdx
        let sym = GrammarSymbol(kind: stTerminal, index: symIndex)
        
        # Use variable name if available, otherwise generic name
        let name = if varName.isSome:
          varName.get()
        else:
          "token_" & $symIndex
        
        # Determine kind based on variable name
        let varKind = if varName.isSome:
          variableTypeForName(varName.get())
        else:
          vtAnonymous
        
        # Use the inner rule as the rule for this lexical variable
        
        # Extract precedence from the token's metadata
        var prec: int32 = 0
        if rule.metadataParams.precedence.kind == pkInteger:
          prec = rule.metadataParams.precedence.intValue
        
        # Check if the inner rule also has metadata with precedence (e.g., token(prec(N, ...)))
        var innerRule = rule.metadataRule[]
        if innerRule.kind == rkMetadata and innerRule.metadataParams.precedence.kind == pkInteger:
          prec = innerRule.metadataParams.precedence.intValue
          innerRule = innerRule.metadataRule[]
        
        if innerRule.kind == rkSymbol and innerRule.symbol.kind == stTerminal:
            let idx = innerRule.symbol.index
            if idx >= 0 and idx < lexicalVars.len.uint16:
               innerRule = lexicalVars[idx].rule
            else:
               discard
        else:
            discard

        lexicalVars.add(LexicalVariable(
          name: name,
          kind: varKind,
          implicitPrecedence: prec, 
          startState: 0,
          rule: innerRule
        ))
        
        lexicalMap[rule] = sym
        return Rule(kind: rkSymbol, symbol: sym)
      else:
        let inner = processRule(rule.metadataRule[], varName)
        Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))
    of rkRepeat:
      let inner = processRule(rule.repeatContent[], varName)
      Rule(kind: rkRepeat, repeatContent: toRef(inner))
    else:
      rule

  # First, process extras to ensure inline patterns/strings are extracted
  for extra in interned.extraSymbols:
    discard processRule(extra)

  for i, variable in interned.variables:
    let processedRule = processRule(variable.rule, some(variable.name))
    
    # Check if this variable's entire rule became a single terminal symbol (token-wrapped)
    if processedRule.kind == rkSymbol and processedRule.symbol.kind == stTerminal:
      # This variable is entirely a token-wrapped rule
      variableToLexicalSymbol[i] = processedRule.symbol
    
    syntaxVars.add(Variable(
      name: variable.name,
      kind: variable.kind,
      rule: processedRule
    ))

  (syntaxVars, lexicalVars, variableToLexicalSymbol)

proc expandRepeats(syntaxVars: seq[Variable]): seq[Variable] =
  ## Expand repeat rules into auxiliary variables using binary tree structure.
  var resultVars = syntaxVars
  var newVars: seq[Variable] = @[]
  
  # Track existing repeats for deduplication - reuse aux variables for identical content
  var existingRepeats = stdtables.initTable[Rule, GrammarSymbol]()
  
  proc toRef(r: Rule): ref Rule =
    new(result)
    result[] = r

  var currentRawIndex = syntaxVars.len
  var repeatCountInVariable = 0
  var currentVariableName = ""

  proc wrapInBinaryTree(auxSymbol: GrammarSymbol, content: Rule): Rule =
    ## Create left-recursive structure: Choice(Seq(Aux, Content), content)
    let symRef = Rule(kind: rkSymbol, symbol: auxSymbol)
    # Aux -> Aux Content | Content
    let seqRule = Rule(kind: rkSeq, seqMembers: @[symRef, content])
    Rule(kind: rkChoice, choiceMembers: @[seqRule, content])

  proc process(rule: Rule, baseName: string): Rule =
    case rule.kind
    of rkChoice:
      var members: seq[Rule] = @[]
      for m in rule.choiceMembers:
        members.add(process(m, baseName))
      Rule(kind: rkChoice, choiceMembers: members)
    of rkSeq:
      var members: seq[Rule] = @[]
      for m in rule.seqMembers:
        members.add(process(m, baseName))
      Rule(kind: rkSeq, seqMembers: members)
    of rkMetadata:
      let inner = process(rule.metadataRule[], baseName)
      Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))
    of rkRepeat:
      # Recursively process the repeat content first
      let content = process(rule.repeatContent[], baseName)
      
      # Check if we've already created an auxiliary for this exact content (deduplication)
      if content in existingRepeats:
        # Reuse existing auxiliary variable
        return Rule(kind: rkSymbol, symbol: existingRepeats[content])
      
      # Create new auxiliary variable for this repeat
      inc repeatCountInVariable
      let auxIndex: uint16 = currentRawIndex.uint16
      inc currentRawIndex
      let auxName = baseName & "_repeat" & $repeatCountInVariable
      let auxSymbol = GrammarSymbol(kind: stNonTerminal, index: auxIndex)
      
      # Binary tree structure using helper function
      let choiceRule = wrapInBinaryTree(auxSymbol, content)
      
      # Register this repeat for deduplication
      existingRepeats[content] = auxSymbol
      
      newVars.add(Variable(
        name: auxName,
        kind: vtAuxiliary,
        rule: choiceRule
      ))
      
      # Return choice of the new aux variable (1+) or blank (epsilon)
      Rule(kind: rkChoice, choiceMembers: @[
        Rule(kind: rkSymbol, symbol: auxSymbol),
        Rule(kind: rkBlank)
      ])
    of rkReserved:
      let inner = process(rule.reservedRule[], baseName)
      Rule(kind: rkReserved, reservedRule: toRef(inner), reservedContextName: rule.reservedContextName)
    else:
      rule

  # Process each variable
  for i in 0 ..< resultVars.len:
    currentVariableName = resultVars[i].name
    repeatCountInVariable = 0
    
    # Special case: hidden variable with top-level repeat
    # Convert in-place to auxiliary instead of creating extra rule
    if resultVars[i].kind == vtHidden and resultVars[i].rule.kind == rkRepeat:
      let content = process(resultVars[i].rule.repeatContent[], resultVars[i].name)
      let auxSymbol = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
      
      # Binary tree structure for the hidden variable itself
      resultVars[i].rule = wrapInBinaryTree(auxSymbol, content)
      resultVars[i].kind = vtAuxiliary  # Convert to auxiliary
      
    else:
      # Normal processing
      resultVars[i].rule = process(resultVars[i].rule, resultVars[i].name)
    
  # Append new auxiliary variables
  resultVars.add(newVars)
  resultVars

proc processInlines(vars: seq[Variable], variablesToInline: seq[GrammarSymbol]): tuple[vars: seq[Variable], indexMap: stdtables.Table[int, int]] =
  # Build map of inline symbols to their rules
  var inlineMap = stdtables.initTable[GrammarSymbol, Rule]()
  var inlineSet = initHashSet[GrammarSymbol]() 
  
  for s in variablesToInline:
    inlineSet.incl(s)
  
  for i, v in vars:
    let sym = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
    if sym in inlineSet:
      inlineMap[sym] = v.rule

  proc toRef(r: Rule): ref Rule =
    new(result)
    result[] = r

  proc process(rule: Rule): Rule =
    case rule.kind
    of rkSymbol:
      if rule.symbol.kind == stNonTerminal and rule.symbol in inlineMap:
        let inlinedRule = inlineMap[rule.symbol]
        # Recursively process the inlined rule content (in case of nested inlines)
        return process(inlinedRule)
      else:
        return rule
    of rkChoice:
      var members: seq[Rule] = @[]
      for m in rule.choiceMembers:
        members.add(process(m))
      Rule(kind: rkChoice, choiceMembers: members)
    of rkSeq:
      var members: seq[Rule] = @[]
      for m in rule.seqMembers:
        members.add(process(m))
      Rule(kind: rkSeq, seqMembers: members)
    of rkMetadata:
      let inner = process(rule.metadataRule[])
      Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))
    of rkRepeat:
      let inner = process(rule.repeatContent[])
      Rule(kind: rkRepeat, repeatContent: toRef(inner))
    of rkReserved:
      let inner = process(rule.reservedRule[])
      Rule(kind: rkReserved, reservedRule: toRef(inner), reservedContextName: rule.reservedContextName)
    else:
      rule

  # First pass: collect non-inlined variables and create index mapping
  var resultVars: seq[Variable] = @[]
  var indexMap = stdtables.initTable[int, int]()  # old index -> new index
  var newIndex = 0
  
  for i, v in vars:
    let sym = GrammarSymbol(kind: stNonTerminal, index: i.uint16)
    if sym notin inlineSet:
      indexMap[i] = newIndex
      inc newIndex
      resultVars.add(Variable(
        name: v.name,
        kind: v.kind,
        rule: process(v.rule)
      ))
  
  # Second pass: remap all symbol indices in all rules
  proc remapSymbols(rule: Rule): Rule =
    case rule.kind
    of rkSymbol:
      if rule.symbol.kind == stNonTerminal:
        # Remap the symbol index
        if rule.symbol.index.int in indexMap:
          return Rule(kind: rkSymbol, symbol: GrammarSymbol(kind: stNonTerminal, index: indexMap[rule.symbol.index.int].uint16))
        else:
          # This symbol was inlined but not replaced - this is an error
          # (Should have been handled by the process() phase, but double check)
          raise newException(CatchableError, "GrammarSymbol should have been replaced during inlining: " & $rule.symbol)
      else:
        return rule
    of rkChoice:
      var members: seq[Rule] = @[]
      for m in rule.choiceMembers:
        members.add(remapSymbols(m))
      Rule(kind: rkChoice, choiceMembers: members)
    of rkSeq:
      var members: seq[Rule] = @[]
      for m in rule.seqMembers:
        members.add(remapSymbols(m))
      Rule(kind: rkSeq, seqMembers: members)
    of rkMetadata:
      let inner = remapSymbols(rule.metadataRule[])
      Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))
    of rkRepeat:
      let inner = remapSymbols(rule.repeatContent[])
      Rule(kind: rkRepeat, repeatContent: toRef(inner))
    of rkReserved:
      let inner = remapSymbols(rule.reservedRule[])
      Rule(kind: rkReserved, reservedRule: toRef(inner), reservedContextName: rule.reservedContextName)
    else:
      rule
  
  # Apply remapping to all variables
  for i in 0 ..< resultVars.len:
    resultVars[i].rule = remapSymbols(resultVars[i].rule)
      
  (resultVars, indexMap)

proc expandNestedChoices(rule: Rule): Rule =
  ## Recursively expand nested choices (e.g. inside sequences or metadata)
  ## to top-level choices.
  proc toRef(r: Rule): ref Rule =
    new(result)
    result[] = r

  case rule.kind
  of rkChoice:
    var members: seq[Rule] = @[]
    for m in rule.choiceMembers:
      let sub = expandNestedChoices(m)
      if sub.kind == rkChoice:
        for subM in sub.choiceMembers:
          members.add(subM)
      else:
        members.add(sub)
    Rule(kind: rkChoice, choiceMembers: members)

  of rkSeq:
    var memberOptions: seq[seq[Rule]] = @[]
    for m in rule.seqMembers:
      let sub = expandNestedChoices(m)
      if sub.kind == rkChoice:
        memberOptions.add(sub.choiceMembers)
      else:
        memberOptions.add(@[sub])
    
    # Cartesian product
    var results: seq[seq[Rule]] = @[ @[] ]
    for options in memberOptions:
      var nextResults: seq[seq[Rule]] = @[]
      for res in results:
        for opt in options:
          var newRes = res
          newRes.add(opt)
          nextResults.add(newRes)
      results = nextResults
    
    # Create result rules
    if results.len == 0:
      Rule(kind: rkSeq, seqMembers: @[])
    elif results.len == 1:
      Rule(kind: rkSeq, seqMembers: results[0])
    else:
      var choices: seq[Rule] = @[]
      for res in results:
        choices.add(Rule(kind: rkSeq, seqMembers: res))
      Rule(kind: rkChoice, choiceMembers: choices)

  of rkMetadata:
    let inner = expandNestedChoices(rule.metadataRule[])
    if inner.kind == rkChoice:
      # Distribute metadata over choice: meta(choice(A, B)) -> choice(meta(A), meta(B))
      var choices: seq[Rule] = @[]
      for m in inner.choiceMembers:
        choices.add(Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(m)))
      Rule(kind: rkChoice, choiceMembers: choices)
    else:
      Rule(kind: rkMetadata, metadataParams: rule.metadataParams, metadataRule: toRef(inner))

  of rkRepeat:
    # Do not lift choices out of repeat! Just process inner content.
    let inner = expandNestedChoices(rule.repeatContent[])
    Rule(kind: rkRepeat, repeatContent: toRef(inner))

  of rkReserved:
    let inner = expandNestedChoices(rule.reservedRule[])
    if inner.kind == rkChoice:
      var choices: seq[Rule] = @[]
      for m in inner.choiceMembers:
        choices.add(Rule(kind: rkReserved, reservedRule: toRef(m), reservedContextName: rule.reservedContextName))
      Rule(kind: rkChoice, choiceMembers: choices)
    else:
      Rule(kind: rkReserved, reservedRule: toRef(inner), reservedContextName: rule.reservedContextName)

  else:
    rule

proc prepareGrammar*(input: InputGrammar): tuple[syntax: SyntaxGrammar, lexical: LexicalGrammar] =
  ## Prepare grammar for table building
  
  # Step 1: Intern symbols
  let interned = internSymbols(input)
  
  # Step 2: Extract tokens
  let (initialSyntaxVars, lexicalVars, variableToLexicalSymbol) = extractTokens(interned)
  
  # Step 3: Process inlines
  # Must be done BEFORE expanding nested choices, because inlining can introduce new nested choices
  let (inlinedSyntaxVars, indexMap) = processInlines(initialSyntaxVars, interned.variablesToInline)
  
  # Step 4: Expand repeats
  # Must be done BEFORE expanding nested choices, because repeat expansion generates new sequences that may contain choices
  let varsWithRepeats = expandRepeats(inlinedSyntaxVars)
  
  # Step 5: Expand nested choices (e.g. inside sequences)
  var finalSyntaxVars: seq[Variable] = @[]
  for v in varsWithRepeats:
    finalSyntaxVars.add(Variable(
      name: v.name,
      kind: v.kind,
      rule: expandNestedChoices(v.rule)
    ))
  
  # Convert external tokens from Variable to ExternalToken
  var externalTokensList: seq[ExternalToken] = @[]
  for extVar in interned.externalTokens:
    externalTokensList.add(ExternalToken(
      name: extVar.name,
      correspondingInternalToken: none(GrammarSymbol)  # External tokens don't map to internal
    ))
  
  # Process extras to get their symbols
  # Extras should be lexical tokens that get automatically skipped
  var extraSymbols: seq[GrammarSymbol] = @[]
  
  proc collectExtras(r: Rule) =
    case r.kind
    of rkSymbol:
      if r.symbol.kind == stTerminal:
        extraSymbols.add(r.symbol)
      elif r.symbol.kind == stNonTerminal:
         # First check if this non-terminal maps to a lexical symbol (token-wrapped)
         let nonTermIdx = r.symbol.index.int
         # IMPORTANT: The nonTermIdx here refers to the ORIGINAL index (from extractTokens), 
         # but variableToLexicalSymbol is keyed by original index.
         # BUT we need to be careful if extra symbols refer to syntax variables that got shifted?
         # Extras are usually terminals or token-wrapped. If token-wrapped, they are processed 
         # in extractTokens before inlining. So indexMap shouldn't affect variableToLexicalSymbol lookup keys
         # IF we used the original index. But wait, `extraSymbols` in `interned` use original indices.
         # We haven't remapped `interned.extraSymbols`.
         if nonTermIdx in variableToLexicalSymbol:
           # This non-terminal is actually a token-wrapped rule, use its lexical symbol
           extraSymbols.add(variableToLexicalSymbol[nonTermIdx])
         elif nonTermIdx < interned.variables.len:
           # Check if the rule is a choice of token-wrapped variables only
           let origVariable = interned.variables[nonTermIdx]
           if origVariable.rule.kind == rkChoice:
             var allTokenWrapped = true
             for member in origVariable.rule.choiceMembers:
               if member.kind == rkSymbol and member.symbol.kind == stNonTerminal:
                 if member.symbol.index.int notin variableToLexicalSymbol:
                   allTokenWrapped = false
                   break
               elif member.kind != rkSymbol:
                 allTokenWrapped = false
                 break
             
             if allTokenWrapped:
               # This is a choice of token-wrapped rules - recurse to expand them
               collectExtras(origVariable.rule)
             else:
               # This has non-token-wrapped elements.
               # If it was inlined, we can't use it as a symbol directly unless we remap it.
               if nonTermIdx in indexMap:
                  extraSymbols.add(GrammarSymbol(kind: stNonTerminal, index: indexMap[nonTermIdx].uint16))
               else:
                  # If it was inlined, it shouldn't be an extra?
                  # Or we should expand it?
                  discard
           else:
             if nonTermIdx in indexMap:
                extraSymbols.add(GrammarSymbol(kind: stNonTerminal, index: indexMap[nonTermIdx].uint16))
      elif r.symbol.kind == stExternal:
         extraSymbols.add(r.symbol)
    of rkChoice:
      # Recurse into each choice member
      for member in r.choiceMembers:
        collectExtras(member)
    of rkMetadata:
      collectExtras(r.metadataRule[])
    of rkString, rkPattern:
      # Inline literal/pattern - find lexical variable
      var found = false
      for i, lexVar in lexicalVars:
        if lexVar.rule == r:
          extraSymbols.add(GrammarSymbol(kind: stTerminal, index: i.uint16))
          found = true
          break
      if not found:
        echo "[WARNING] Extra rule not found in lexical variables: ", r
    of rkSeq:
       echo "[WARNING] Unexpected seq in extras: ", r
    else:
      discard

  for extraRule in interned.extraSymbols:
    collectExtras(extraRule)

  # Smart Extras: Resolve conflicts between default extras and user tokens
  # If the user is using the default extras ([\s]), and they also define a token
  # that matches whitespace (e.g. `space: $ => / /`), we should remove the default extra
  # to avoid ambiguity/conflicts.
  proc detectAndResolveExtrasConflicts(extras: var seq[GrammarSymbol], lexicalVars: seq[LexicalVariable]) =
    # 1. Check if extras is exactly the default `[\s]`
    if extras.len != 1: return
    
    let extraSym = extras[0]
    if extraSym.kind != stTerminal: return
    
    let extraVar = lexicalVars[extraSym.index.int]
    
    # Check if this extra is `/\s/`
    var isDefaultExtra = false
    if extraVar.rule.kind == rkPattern and extraVar.rule.patternValue == "\\s":
      isDefaultExtra = true
    
    if not isDefaultExtra: return
    
    # 2. Check for conflicting user tokens
    var hasConflict = false
    for i, lexVar in lexicalVars:
      # Skip the extra itself
      if i.uint16 == extraSym.index: continue
      
      # Check for whitespace overlap
      var overlaps = false
      
      if lexVar.rule.kind == rkString:
        # Check if string literal contains whitespace
        for c in lexVar.rule.stringValue:
          if c in {' ', '\t', '\r', '\n'}:
            overlaps = true
            break
            
      elif lexVar.rule.kind == rkPattern:
        # Check if pattern implies whitespace
        # Heuristic: contains \s or space char
        if lexVar.rule.patternValue.contains("\\s") or 
           lexVar.rule.patternValue.contains(" ") or
           lexVar.rule.patternValue.contains("\\t") or
           lexVar.rule.patternValue.contains("\\r") or
           lexVar.rule.patternValue.contains("\\n"):
          overlaps = true
      
      if overlaps:
        hasConflict = true
        echo "[Treestand] Removing default extra `\\s` because it conflicts with user token: ", lexVar.name
        break
    
    if hasConflict:
      extras.setLen(0)

  detectAndResolveExtrasConflicts(extraSymbols, lexicalVars)
  
  # Resolve named precedences
  proc resolveNamedPrecedences(orderings: seq[seq[PrecedenceEntry]]): stdtables.Table[string, int32] =
    result = stdtables.initTable[string, int32]()
    var changed = true
    var iterations = 0
    
    # Initialize all mentioned names with 1 (to distinguish from default 0)
    for ordering in orderings:
      for entry in ordering:
        let name = if entry.isName: entry.name else: entry.symbol
        if name notin result:
          result[name] = 1

    # Iteratively satisfy constraints: A > B => Val(A) > Val(B)
    while changed and iterations < 100:
      changed = false
      iterations.inc
      
      for ordering in orderings:
        if ordering.len < 2: continue
        for i in 0 ..< ordering.len - 1:
          let high = if ordering[i].isName: ordering[i].name else: ordering[i].symbol
          let low = if ordering[i+1].isName: ordering[i+1].name else: ordering[i+1].symbol
          
          if result[high] <= result[low]:
            result[high] = result[low] + 1
            changed = true
            
    if changed:
      echo "[WARNING] Precedence ordering likely has cycles or is too deep."

  let precedenceMap = resolveNamedPrecedences(interned.precedenceOrderings)

  # --- Remapping logic for Conflicts, Supertypes, and Word Token ---
  # These were parsed referencing original variable indices. Inlining shifted those indices.
  # We must remap them using the indexMap returned by processInlines.

  proc remapSymbol(s: GrammarSymbol): Option[GrammarSymbol] =
    if s.kind == stNonTerminal:
      if s.index.int in indexMap:
        some(GrammarSymbol(kind: stNonTerminal, index: indexMap[s.index.int].uint16)) 
      else:
        # Symbol was inlined. It typically shouldn't appear in conflicts if it doesn't exist anymore.
        none(GrammarSymbol) 
    else:
      some(s)

  var remappedConflicts: seq[seq[GrammarSymbol]] = @[]
  for conflictSet in interned.expectedConflicts:
    var newSet: seq[GrammarSymbol] = @[]
    for s in conflictSet:
      let r = remapSymbol(s)
      if r.isSome: newSet.add(r.get)
    if newSet.len > 0:
      remappedConflicts.add(newSet)

  var remappedSupertypes: seq[GrammarSymbol] = @[]
  for s in interned.supertypeSymbols:
    let r = remapSymbol(s)
    if r.isSome: remappedSupertypes.add(r.get)

  var remappedWordToken = interned.wordToken
  if remappedWordToken.isSome:
    let r = remapSymbol(remappedWordToken.get)
    remappedWordToken = r

  var syntaxGrammar = SyntaxGrammar(
    variables: @[],
    extraSymbols: extraSymbols,  
    expectedConflicts: remappedConflicts, # Use remapped
    externalTokens: externalTokensList,
    supertypeSymbols: remappedSupertypes, # Use remapped
    variablesToInline: interned.variablesToInline,
    wordToken: remappedWordToken,         # Use remapped
    precedenceOrderings: interned.precedenceOrderings,
    reservedWordSets: @[]
  )
  
  # Convert variables to syntax variables (simplified)
  for variable in finalSyntaxVars:
    var productions: seq[Production] = @[]
    let choices = extractChoices(variable.rule)
    
    for choice in choices:
      var steps: seq[ProductionStep] = @[]
      var precedenceStack: seq[Precedence] = @[]
      var associativityStack: seq[GrammarAssociativity] = @[]
      var aliasStack: seq[Alias] = @[]
      var fieldNameStack: seq[string] = @[]
      var reservedWordStack: seq[ReservedWordSetId] = @[]
      var dynamicPrec: int32 = 0
      
      proc apply(r: Rule, atEnd: bool): bool =
        case r.kind
        of rkSeq:
          var didPush = false
          let lastIndex = r.seqMembers.len - 1
          for i, member in r.seqMembers:
            let p = apply(member, i == lastIndex and atEnd)
            didPush = didPush or p
          return didPush
        
        of rkMetadata:
          var hasPrec = false
          if r.metadataParams.precedence.kind != pkNone:
            hasPrec = true
            precedenceStack.add(r.metadataParams.precedence)
          
          var hasAssoc = false
          if r.metadataParams.associativity.isSome:
            hasAssoc = true
            associativityStack.add(r.metadataParams.associativity.get)
          
          var hasAlias = false
          if r.metadataParams.alias.isSome:
            hasAlias = true
            aliasStack.add(r.metadataParams.alias.get)
          
          var hasFieldName = false
          if r.metadataParams.fieldName.isSome:
            hasFieldName = true
            fieldNameStack.add(r.metadataParams.fieldName.get)
          
          if abs(r.metadataParams.dynamicPrecedence) > abs(dynamicPrec):
            dynamicPrec = r.metadataParams.dynamicPrecedence
          
          let didPush = apply(r.metadataRule[], atEnd)
          
          if hasPrec:
            discard precedenceStack.pop()
            if didPush and not atEnd:
              steps[^1].precedence = if precedenceStack.len > 0:
                precedenceStack[^1]
              else:
                nonePrecedence()
          
          if hasAssoc:
            discard associativityStack.pop()
            if didPush and not atEnd:
              steps[^1].associativity = if associativityStack.len > 0:
                some(associativityStack[^1])
              else:
                none(GrammarAssociativity)
          
          if hasAlias:
            discard aliasStack.pop()
          
          if hasFieldName:
            discard fieldNameStack.pop()
          
          return didPush
        
        of rkReserved:
          var setId = NoReservedWords
          for i, reservedSet in interned.reservedWordSets:
            if reservedSet.name == r.reservedContextName:
              setId = ReservedWordSetId(i.uint)
              break
          
          reservedWordStack.add(setId)
          let didPush = apply(r.reservedRule[], atEnd)
          discard reservedWordStack.pop()
          return didPush
        
        of rkSymbol:
          steps.add(ProductionStep(
            symbol: r.symbol,
            precedence: if precedenceStack.len > 0: 
              precedenceStack[^1]
            else:
              nonePrecedence(),
            associativity: if associativityStack.len > 0:
              some(associativityStack[^1])
            else:
              none(GrammarAssociativity),
            reservedWordSetId: if reservedWordStack.len > 0:
              reservedWordStack[^1]
            else:
              NoReservedWords,
            alias: if aliasStack.len > 0:
              some(aliasStack[^1])
            else:
              none(Alias),
            fieldName: if fieldNameStack.len > 0:
              some(fieldNameStack[^1])
            else:
              none(string)
          ))
          return true
        
        else:
          return false
      
      discard apply(choice, true)
      
      var prodPrec: int32 = 0
      var prodAssoc: Option[GrammarAssociativity] = none(GrammarAssociativity)
      
      if steps.len > 0:
        let lastStep = steps[^1]
        prodAssoc = lastStep.associativity
        if lastStep.precedence.kind == pkInteger:
          prodPrec = lastStep.precedence.intValue
        elif lastStep.precedence.kind == pkName:
          if lastStep.precedence.name in precedenceMap:
            prodPrec = precedenceMap[lastStep.precedence.name]
      
      productions.add(Production(
        steps: steps, 
        dynamicPrecedence: dynamicPrec,
        precedence: prodPrec,
        associativity: prodAssoc
      ))
    
    syntaxGrammar.variables.add(SyntaxVariable(
      name: variable.name,
      kind: variable.kind,
      productions: productions
    ))
  
  # Create lexical grammar
  var lexicalGrammar = LexicalGrammar(
    nfa: newNfa(),
    variables: lexicalVars
  )
  
  # Validate: Check for epsilon rules (rules that match empty string)
  proc canMatchEmpty(varIdx: int, visited: var HashSet[int]): bool =
    if varIdx in visited:
      return false 
    visited.incl(varIdx)
    
    if varIdx < 0 or varIdx >= syntaxGrammar.variables.len:
      return false
    
    let variable = syntaxGrammar.variables[varIdx]
    
    for production in variable.productions:
      var allStepsCanBeEmpty = true
      for step in production.steps:
        case step.symbol.kind
        of stNonTerminal:
          if not canMatchEmpty(step.symbol.index.int, visited):
            allStepsCanBeEmpty = false
            break
        of stTerminal, stExternal, stEnd, stEndOfNonTerminalExtra:
          allStepsCanBeEmpty = false
          break
      
      if allStepsCanBeEmpty:
        visited.excl(varIdx) 
        return true
    
    visited.excl(varIdx)
    return false
  
  for i in 1..<syntaxGrammar.variables.len: 
    let ruleName = syntaxGrammar.variables[i].name
    
    if ruleName.find("_repeat") != -1 or ruleName.find("_seq") != -1:
      continue
    
    var visited = initHashSet[int]()
    if canMatchEmpty(i, visited):
      raise newException(PrepareGrammarError, 
        "The rule `" & ruleName & "` matches the empty string.\n\n" &
        "Tree-sitter does not support syntactic rules that match the empty string\n" &
        "unless they are used only as the grammar's start rule.")
  
  result = (syntax: syntaxGrammar, lexical: lexicalGrammar)