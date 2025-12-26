
import std/[os, macros, strformat, strutils, tables, compilesettings]
import ./[js_exec, dsl, parse_grammar, prepare_grammar, build_tables, codegen, grammar]
{.warning[UnusedImport]: off.}

macro importGrammar*(grammar: static[string]): untyped =
  ## Compile-time macro that imports a Tree-sitter grammar and generates a complete
  ## parser implementation directly into your Nim module.
  ##
  ## This macro performs the entire parser generation pipeline at compile-time:
  ## 1. Executes the grammar.js file to obtain the grammar definition
  ## 2. Parses and prepares the grammar rules
  ## 3. Builds LR parsing tables
  ## 4. Generates optimized parser code
  ## 5. Injects the generated code into your module
  ##
  ## Parameters
  ## ----------
  ## grammar : static[string]
  ##   Path to the Tree-sitter grammar.js file. Can be absolute or relative.
  ##   Supports tilde expansion (e.g., "~/grammars/json/grammar.js").
  ##
  ## Generated API
  ## -------------
  ## After calling `importGrammar`, your module will have access to:
  ## - `parse<GrammarName>(input: string): Node` - Parse a string and return the syntax tree
  ##   where <GrammarName> is converted to CamelCase (e.g., "json" → parseJson,
  ##   "unused_rules" → parseUnusedRules, "my_lang" → parseMyLang)
  ## - `newParser(input: string): Parser` - Create a parser instance (same for all grammars)
  ## - All types and procedures defined in the generated parser runtime
  ##
  ## Example
  ## -------
  ## ```nim
  ## import treestand
  ##
  ## # Import JSON grammar at compile-time
  ## importGrammar("path/to/json/grammar.js")
  ##
  ## # Use the generated parser
  ## let tree = parseJson("""{"key": "value"}""")
  ## echo tree  # Prints the parse tree
  ## ```
  ##
  ## Relative Paths
  ## --------------
  ## You can use `currentSourcePath` to reference grammars relative to your source file:
  ## ```nim
  ## import std/os
  ## importGrammar(currentSourcePath.parentDir / "grammar.js")
  ## ```
  ##
  ## Compile-Time vs Runtime
  ## -----------------------
  ## - **Compile-time**: Grammar processing, table generation, code generation
  ## - **Runtime**: Only the actual parsing of input strings
  ## - This results in zero runtime overhead for grammar compilation
  ##
  ## Notes
  ## -----
  ## - The grammar path must be known at compile-time (static[string])
  ## - Node.js (or Bun/Deno) must be available to execute grammar.js files
  ## - The generated parser code is directly embedded in your module
  ## - Each `importGrammar` call generates a complete parser implementation
  
  # VM implementation: sooooooooooooo slow

  # let dslPath = currentSourcePath.parentDir / "dsl.js"
  # let grammarPath = grammar.expandTilde()
  # echo "[Treestand] building from grammar: " & grammar
  # let grammarJson = executeGrammarJsStatic(grammarPath, dslPath)
  # let inputGrammar = parseGrammar(grammarJson)
  # let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
  # let tables = buildTables(syntaxGrammar, lexicalGrammar)
  # let parserName = inputGrammar.name
  # var externalTokens: seq[string] = @[]
  # for ext in syntaxGrammar.externalTokens:
  #   externalTokens.add(ext.name)
  # let parserCode = generateParser(
  #   parserName,
  #   grammarPath.parentDir(),
  #   syntaxGrammar,
  #   lexicalGrammar,
  #   tables.parseTable,
  #   tables.mainLexTable,
  #   externalTokens)
  # return parserCode.parseStmt()

  # Static run implementation
  let libSourceDir = currentSourcePath.parentDir.parentDir
  var
    output: string
    exitCode: int
  
  when defined(debug):
    let cmd = fmt"""nim r -d:debug --path:"{libSourceDir}" "{libSourceDir / "treestand.nim"}" --cmd generate --grammar_path "{grammar.expandTilde()}" """
  else:
    let treestand_cmd = findExeStatic("treestand")
    var success: bool = false
    if treestand_cmd.len > 0:
      let cmd = fmt"""{treestand_cmd} --cmd generate --grammar_path "{grammar.expandTilde()}" """ # Why the fuck this fails on windows? Windows is just the worst in the world.
      echo "[Treestand] Running: " & cmd
      (output, exitcode) = gorgeEx(cmd)
      if exitcode == 0:
        success = true
    if not success:
      let cmd = fmt"""nim r -d:release --path:"{libSourceDir}" "{libSourceDir / "treestand.nim"}" --cmd generate --grammar_path "{grammar.expandTilde()}" """
      echo "[Treestand] Running: " & cmd
      (output, exitcode) = gorgeEx(cmd)
      if exitcode == 0:
        success = true
      else:
        raise newException(Exception, "Failed to generate parser. Output:\n" & output)

  return output[output.find("# GENERATED PARSER.NIM") ..< output.len].parseStmt()

macro buildGrammar*(createGrammarFunction: untyped): untyped =
  ## Compile-time macro that builds a parser from a pure Nim grammar definition.
  ## 
  ## This is a **meta-macro** - it generates another macro that performs the actual
  ## parser generation. This allows you to define grammars entirely in Nim using the
  ## DSL, without needing any JavaScript grammar.js files.
  result = quote do:
    macro buildGrammarImpl(): untyped =
      let inputGrammar = `createGrammarFunction`()
      let (syntaxGrammar, lexicalGrammar) = prepareGrammar(inputGrammar)
      let tables = buildTables(syntaxGrammar, lexicalGrammar)
      let parserName = inputGrammar.name
      var externalTokens: seq[string] = @[]
      for ext in syntaxGrammar.externalTokens: externalTokens.add(ext.name)
      let parserCode = generateParser(
        parserName,
        "",
        syntaxGrammar,
        lexicalGrammar,
        tables.parseTable,
        tables.mainLexTable,
        externalTokens
      )
      # echo parserCode
      return parserCode.parseStmt()
    buildGrammarImpl()
  # echo result.repr

proc transformRule(n: NimNode): NimNode =
  ## Transforms Nim AST expressions into DSL function calls.
  case n.kind
  of nnkInfix:
    let op = n[0].strVal
    let left = transformRule(n[1])
    let right = transformRule(n[2])
    case op
    of "*": # Sequence
      result = quote do:
        seq(`left`, `right`)
    of "|": # Choice
      result = quote do:
        choice(`left`, `right`)
    of "^": # Precedence Left (default for ^)
      # n[2] should be integer
      result = quote do:
        prec_left(`right`.int, `left`) 
    else:
      error("Unsupported infix operator in grammar rule: " & op, n)

  of nnkPrefix:
    let op = n[0].strVal
    let child = transformRule(n[1])
    case op
    of "+": # Repetition (One or more)
      result = quote do:
        seq(`child`, rep(`child`))
    of "*": # Zero or more
      result = quote do:
        rep(`child`)
    of "?": # Optional
      result = quote do:
        opt(`child`)
    of ">": # Capture (npeg style) - ignored, just return child
      result = child
    else:
      error("Unsupported prefix operator in grammar rule: " & op, n)

  of nnkIdent:
    # Symbol reference
    let s = n.strVal
    result = quote do:
      sym(`s`)

  of nnkCall:
    # Function call like token(...), prec(...)
    # We preserve the call but transform arguments
    var newCall = newCall(n[0])
    for i in 1 ..< n.len:
      newCall.add transformRule(n[i])
    result = newCall

  of nnkStrLit, nnkTripleStrLit:
    # String literal
    result = quote do:
      str(`n`)
      
  of nnkIntLit:
    # Integer literal (used in precedence)
    result = n

  of nnkCallStrLit: 
    # re"..." -> patt("...")
    if n[0].strVal == "re":
      let p = n[1].strVal
      result = quote do:
        patt(`p`)
    else:
      error("Unsupported string literal wrapper: " & n[0].strVal, n)
  
  of nnkPar:
    # Parentheses: (expr)
    if n.len == 1:
      result = transformRule(n[0])
    else:
      error("Parentheses in rule must contain exactly one expression", n)
  
  of nnkTupleConstr:
    # (name: rule) -> field("name", rule)
    if n.len == 1 and n[0].kind == nnkExprColonExpr:
      let key = n[0][0]
      let val = transformRule(n[0][1])
      let keyStr = key.strVal
      result = quote do:
        field(`keyStr`, `val`)
    else:
      error("Named field must be in format (name: rule)", n)

  of nnkCurly:
    # {"a", "b"} -> choice(str("a"), str("b"))
    var choices = newCall("choice")
    for child in n:
       if child.kind in {nnkStrLit, nnkTripleStrLit}:
         choices.add quote do:
           str(`child`)
       else:
         error("Set syntax { ... } only supports string literals for keywords", child)
    result = choices

  else:
    # Pass through other nodes (like int literals in prec calls) if simple
    if n.kind == nnkIntLit:
      return n
    error("Unsupported syntax in grammar rule: " & $n.kind, n)
  
proc extractFields(ruleNode: NimNode, fields: var seq[string]) =
  ## Recursively extracts field names from a rule DSL AST
  if ruleNode.kind == nnkCall and ruleNode.len > 0:
    var funcName = ""
    if ruleNode[0].kind == nnkIdent:
       funcName = ruleNode[0].strVal
    elif ruleNode[0].kind == nnkSym:
       funcName = ruleNode[0].strVal
       
    if funcName == "field":
      # field("name", rule)
      if ruleNode.len >= 2 and ruleNode[1].kind in {nnkStrLit, nnkTripleStrLit}:
        fields.add ruleNode[1].strVal
      # Continue searching in inner rule just in case? Usually field wraps logic.
      if ruleNode.len >= 3:
        extractFields(ruleNode[2], fields)
    else:
      # Recurse args (e.g. seq, choice)
      for i in 1 ..< ruleNode.len:
        extractFields(ruleNode[i], fields)
  elif ruleNode.kind in {nnkStmtList, nnkBlockStmt}:
      for child in ruleNode:
          extractFields(child, fields)
          
macro tsGrammarImpl(name: static string, userdata: untyped, body: untyped): untyped =
  ## Implementation of tsGrammar that handles optional userdata
  
  var variables = newNimNode(nnkBracket) # @[...]
  var extraSymbols = newNimNode(nnkBracket)
  var externalTokens = newNimNode(nnkBracket)
  var expectedConflicts = newNimNode(nnkBracket)
  var wordToken = quote do:
    none(string)
  var variablesToInline = newNimNode(nnkBracket)
  var supertypeSymbols = newNimNode(nnkBracket)

  var ruleActions = initTable[string, NimNode]()
  var ruleFields = initTable[string, seq[string]]()

  # echo "Processing tsGrammar body:"
  # echo body.treeRepr

  for stmt in body:
    if stmt.kind == nnkInfix and stmt[0].strVal == "<-":
      # Rule Assignment: name <- rule
      # Check if there is an attached block: name <- rule: action
      # Infix(<-) with 4 children if call syntax used? 
      # Based on ast_check, Infix(...) has 4 children if block attached to the op call?
      # `program <- expression: ...` -> Infix(<- program expression StmtList)
      
      let nameNode = stmt[1]
      let ruleNode = stmt[2] # The rule definition
      var actionBlock: NimNode = nil
      
      if stmt.len >= 4:
         actionBlock = stmt[3]

      let nameStr = nameNode.strVal
      
      if ruleNode.kind == nnkIdent and ruleNode.strVal == "external_token":
        let symNode = quote do:
          sym(`nameStr`)
        externalTokens.add symNode
      else:
        let transformedRule = transformRule(ruleNode)
        let varConstr = quote do:
          Variable(name: `nameStr`, kind: vtNamed, rule: `transformedRule`)
        variables.add varConstr
        
        # Determine fields
        var fields: seq[string] = @[]
        extractFields(transformedRule, fields)
        if fields.len > 0:
           ruleFields[nameStr] = fields
        
        if actionBlock != nil:
           ruleActions[nameStr] = actionBlock

    elif stmt.kind == nnkAsgn and stmt[0].kind == nnkIdent:
      let field = stmt[0].strVal
      let value = stmt[1]
      case field
      of "extras":
        if value.kind == nnkBracket:
          for child in value:
            extraSymbols.add transformRule(child)
        else:
          extraSymbols.add transformRule(value)
      of "conflicts": expectedConflicts = value 
      of "scanner": discard
      of "inline": variablesToInline = value
      of "supertypes": supertypeSymbols = value
      of "word": wordToken = newCall("some", value)
      else: error("Unknown configuration property: " & field, stmt)
    else:
      # echo "Ignored stmt: " & stmt.repr
      discard # Ignore comments or bad syntax

  if externalTokens.len > 0:
    echo "[Treestand] Found external tokens for grammar `" & name & "`: " & externalTokens.repr
  
  # echo "Variables: " & variables.repr

  let grammarProcName = ident(name)
  let buildCall = quote do:
    proc `grammarProcName`*(): InputGrammar =
      InputGrammar(
        name: `name`,
        variables: @`variables`,
        extraSymbols: if (@`extraSymbols`).len > 0: @`extraSymbols` else: @[patt("\\s")],
        expectedConflicts: @`expectedConflicts`,
        externalTokens: @`externalTokens`,
        variablesToInline: @`variablesToInline`,
        supertypeSymbols: @`supertypeSymbols`,
        wordToken: `wordToken`
      )
    buildGrammar(`grammarProcName`)
  
  # Generate Match Proc if userdata provided
  var matchImpl = newStmtList()
  if userdata.kind != nnkNilLit:
     let matchName = ident("match" & name.capitalizeAscii())
     let parserCtor = ident("parse" & name.capitalizeAscii())
     var userDataType = userdata
     if userdata.kind == nnkExprColonExpr:
         userDataType = userdata[1]
     
     # Generate case statement for actions
     var caseStmt = newNimNode(nnkCaseStmt)
     let treeSym = ident("tree")
     let inputSym = ident("input")
     let userdataSym = ident("userdata")
     let nodeSym = ident("node") 

     # Cast node.symbol.nonTerminalIndex to NonTerminalSymbol
     caseStmt.add quote do:
        NonTerminalSymbol(node.symbol.nonTerminalIndex)
     
     var hasActions = false
     for ruleName, actionBody in ruleActions:
        let ntName = ident("nt" & ruleName.capitalizeAscii()) # Assumes enum naming follows this
        
        # Inject standard helpers
        let nodeSym = ident("node")
        let inputSym = ident("input")
        let injectedAction = newStmtList()
        injectedAction.add quote do:
           template capture(i: int): string {.dirty.} = 
             if i > `nodeSym`.children.len or i < 1: "" 
             else:
               let c = `nodeSym`.children[i-1]
               if c.token.text.len > 0: c.token.text
               else: `inputSym`[c.startPos ..< c.endPos]
           
           template child(i: int): ParseNode {.dirty.} =
             if i > `nodeSym`.children.len or i < 1: nil
             else: `nodeSym`.children[i-1]
        
        # Inject field helpers if fields exist
        if ruleFields.hasKey(ruleName):
           let fields = ruleFields[ruleName]
           
           var branchCase = newNimNode(nnkCaseStmt)
           branchCase.add ident("n")
           for idx, fname in fields:
              branchCase.add newTree(nnkOfBranch, newLit(fname), newLit(idx + 1)) # 1-based index? No fields map to specific children.
        
        # Expose standard symbols to user action block
        # We need to use `template` or `let` to alias the hygienic symbols to user-friendly names
        injectedAction.add quote do:
           let node = `nodeSym`
           var userdata = `userdataSym`
           let input = `inputSym`
        
        injectedAction.add actionBody
        caseStmt.add newTree(nnkOfBranch, ntName, injectedAction)
        hasActions = true
     
     if hasActions:
       let discardStmt = quote do: discard
       caseStmt.add newTree(nnkElse, newStmtList(discardStmt))
       
       let traverseBody = quote do:
          proc traverse(`nodeSym`: ParseNode, `userdataSym`: var `userDataType`, `inputSym`: string) =
             if `nodeSym` == nil: return
             for child in `nodeSym`.children: traverse(child, `userdataSym`, `inputSym`)
             
             if `nodeSym`.symbol.kind == skNonTerminal:
                `caseStmt`
          
          traverse(`treeSym`, `userdataSym`, `inputSym`)
          return true

       # Add the traverse proc to matchBody
       matchImpl.add traverseBody
     else:
       matchImpl.add quote do: return true # No actions
     
     let fullMatchProc = quote do:
        proc `matchName`*(`inputSym`: string, `userdataSym`: var `userDataType`): bool =
           var parser = newParser(`inputSym`)
           let `treeSym` = parser.parse()
           if `treeSym` == nil: return false
           `matchImpl`
     
     buildCall.add fullMatchProc

  result = buildCall
  # echo "=================="
  # echo result.repr
  # echo "=================="

macro tsGrammar*(name: static string, arg2: untyped, arg3: untyped = nil): untyped =
  ## Define a grammar using the concise `tsGrammar` DSL.
  ## 
  ## This macro provides a clean, PEG/EBNF-like syntax for defining grammars in pure Nim.
  ## It generates an `InputGrammar` object and optionally a `match<Name>` procedure
  ## if actions are provided.
  ## 
  ## Basic Usage (No Actions)
  ## =========================
  ## 
  ## .. code-block:: nim
  ##   import treestand
  ##   
  ##   tsGrammar "myGrammar":
  ##     program <- +statement
  ##     statement <- ident * eq * expression * semi
  ##     expression <- number | ident | binary
  ##     binary <- (expression * op * expression) ^ 1
  ##     
  ##     ident <- token(re"[a-zA-Z_]\\w*")
  ##     number <- token(re"\\d+")
  ##     eq <- token("=")
  ##     semi <- token(";")
  ##     extras = token(re"\\s+")
  ##   
  ##   let tree = parseMyGrammar("x = 10;")
  ## 
  ## Usage with Embedded Actions
  ## ============================
  ## 
  ## .. code-block:: nim
  ##   type Env = object
  ##     vars: Table[string, int]
  ##   
  ##   tsGrammar "calc", userdata: Env:
  ##     assign <- ident * eq * number * semi:
  ##       let varName = node.child("ident").text
  ##       let value = parseInt(node.child("number").text)
  ##       userdata.vars[varName] = value
  ##     
  ##     ident <- token(re"[a-zA-Z_]\\w*")
  ##     number <- token(re"\\d+")
  ##   
  ##   var env = Env()
  ##   if matchCalc("x = 10;", env):
  ##     echo env.vars
  ## 
  ## Available Symbols in Actions
  ## ----------------------------
  ## 
  ## - `node: ParseNode` - The matched parse node
  ## - `userdata: var YourType` - Mutable userdata object
  ## - `input: string` - Original input string
  ## 
  ## Node API: `node.children`, `node.child(i)`, `node.child("name")`, 
  ## `node.childCount()`, `node.text`, `node.kind`
  ## 
  ## Operators
  ## =========
  ## 
  ## - `rule <- definition` - Define rule
  ## - `a * b` - Sequence
  ## - `a | b` - Choice
  ## - `+a` - One or more
  ## - `?a` - Optional
  ## - `(name: rule)` - Named field
  ## - `rule ^ N` - Precedence level N
  ## - `{"a", "b"}` - String choice
  ## 
  ## See `docs/using_dsl.md` and `examples/09_ast_actions` for complete documentation.
  ## overload for tsGrammar
  var actualUserdata: NimNode = newNilLit()
  var actualBody: NimNode = newNilLit()
  
  # echo "DEBUG tsGrammar arg2 kind: ", arg2.kind
  # echo arg2.treeRepr

  if arg3.kind != nnkNilLit:
     # Standard case: tsGrammar("n", u: string, body)
     # OR tsGrammar("n", userdata: Type: body) parsed as (userdata, StmtList(Call(Type, StmtList)))
     
     # Check if arg3 is StmtList with one child being the Call
     var potentialCall = arg3
     if arg3.kind == nnkStmtList and arg3.len == 1:
        potentialCall = arg3[0]
        
     if arg2.kind == nnkIdent and potentialCall.kind == nnkCall and potentialCall.len == 2 and potentialCall[1].kind == nnkStmtList:
        # arg2="userdata", potentialCall=Call(Type, StmtList)
        actualUserdata = newTree(nnkExprColonExpr, arg2, potentialCall[0])
        actualBody = potentialCall[1]
     else:
        actualUserdata = arg2
        actualBody = arg3
  else:
     # Case: tsGrammar("n", body) OR tsGrammar("n", u: Type: body)
     if arg2.kind == nnkStmtList:
        actualBody = arg2
     elif arg2.kind == nnkExprColonExpr:
        # userdata: (Type: body) -> ExprColonExpr(userdata, Call(Type, StmtList))
        let key = arg2[0]
        let val = arg2[1]
        if val.kind == nnkCall and val.len == 2 and val[1].kind == nnkStmtList:
           # Found hidden body!
           actualUserdata = newTree(nnkExprColonExpr, key, val[0])
           actualBody = val[1]
        else:
             # Just userdata supplied? e.g. tsGrammar("n", u: T)
             actualUserdata = arg2
     elif arg2.kind == nnkCall and arg2.len == 2 and arg2[1].kind == nnkStmtList:
         # Case: tsGrammar "n", Type: body -> Call(Type, StmtList)
         actualUserdata = arg2[0]
         actualBody = arg2[1]
     else:
         # Maybe arg2 is just the body but weirdly parsed? 
         # Or it's a single argument "userdata" with no body?
         actualUserdata = arg2

  result = quote do:
      tsGrammarImpl(`name`, `actualUserdata`, `actualBody`)
