import std/[os, macros, strformat, strutils]
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
  when defined(debug):
    let cmd = fmt"""nim r -d:debug --path:{libSourceDir} {libSourceDir / "treestand.nim"} --cmd generate --grammar_path {grammar.expandTilde()} """
  else:
    let treestand_cmd = findExeStatic("treestand")
    var cmd: string
    if treestand_cmd.len > 0:
      cmd = fmt"""{treestand_cmd} --cmd generate --grammar_path {grammar.expandTilde()} """
    else:
      cmd = fmt"""nim r -d:release --path:{libSourceDir} {libSourceDir / "treestand.nim"} --cmd generate --grammar_path {grammar.expandTilde()} """
  echo "[Treestand] Running: " & cmd
  let (output, exitcode) = gorgeEx(cmd)
  if exitcode != 0:
    raise newException(Exception, "Failed to generate parser from " & grammar)
  return output[output.find("# GENERATED PARSER.NIM") ..< output.len].parseStmt()

macro buildGrammar*(createGrammarFunction: untyped): untyped =
  ## Compile-time macro that builds a parser from a pure Nim grammar definition.
  ## 
  ## This is a **meta-macro** - it generates another macro that performs the actual
  ## parser generation. This allows you to define grammars entirely in Nim using the
  ## DSL, without needing any JavaScript grammar.js files.
  ##
  ## How It Works
  ## ------------
  ## 1. You define a function that returns an `InputGrammar` using DSL functions
  ## 2. `buildGrammar` generates a temporary macro that calls your function
  ## 3. The temporary macro builds the parser at compile-time
  ## 4. The parser code is injected into your module
  ##
  ## Parameters
  ## ----------
  ## createGrammarFunction : untyped
  ##   The name of a function (as an identifier) that returns `InputGrammar`.
  ##   The function will be called at compile-time to obtain the grammar definition.
  ##
  ## Generated API
  ## -------------
  ## After calling `buildGrammar`, your module will have access to:
  ## - `parse<GrammarName>(input: string): ParseNode` - Parse a string (CamelCase naming)
  ## - `newParser(input: string): Parser` - Create a parser instance
  ## - All types and procedures from the generated parser runtime
  ##
  ## DSL Functions
  ## -------------
  ## Use these to define your grammar (from `treestand/dsl`):
  ## - `sym(name)` - Reference another rule by name
  ## - `seq(items...)` - Sequence of elements
  ## - `choice(items...)` - Alternatives (OR)
  ## - `rep(item)` - One or more repetitions
  ## - `rep0(item)` - Zero or more repetitions
  ## - `opt(item)` - Optional element
  ## - `token(rule)` - Mark as lexical token
  ## - `patt(regex)` - Regular expression pattern
  ## - `str(text)` - Literal string
  ## - `prec_left(n, rule)` - Left associativity with precedence
  ## - `prec_right(n, rule)` - Right associativity with precedence
  ## - `prec(n, rule)` - Precedence without associativity
  ## - `prec_dynamic(n, rule)` - Dynamic precedence
  ##
  ## Example
  ## -------
  ## ```nim
  ## import treestand
  ## import std/options
  ##
  ## proc createMathGrammar(): InputGrammar =
  ##   InputGrammar(
  ##     name: "math",
  ##     variables: @[
  ##       Variable(name: "program", kind: vtNamed, 
  ##                rule: rep(sym("expression"))),
  ##       Variable(name: "expression", kind: vtNamed,
  ##                rule: choice(sym("number"), sym("binary_op"))),
  ##       Variable(name: "binary_op", kind: vtNamed,
  ##                rule: prec_left(1, seq(sym("expression"), 
  ##                                      sym("op"), 
  ##                                      sym("expression")))),
  ##       Variable(name: "number", kind: vtNamed,
  ##                rule: token(patt("\\d+"))),
  ##       Variable(name: "op", kind: vtNamed,
  ##                rule: token(patt("[+\\-*/]")))
  ##     ],
  ##     extraSymbols: @[token(patt("\\s+"))]
  ##   )
  ##
  ## buildGrammar(createMathGrammar)
  ##
  ## when isMainModule:
  ##   let tree = parseMath("1 + 2 * 3")
  ##   echo tree
  ## ```
  ##
  ## Advantages
  ## ----------
  ## - **Pure Nim**: No JavaScript dependencies at all
  ## - **Type Safety**: Grammar definition is type-checked by Nim compiler
  ## - **IDE Support**: Full autocomplete and error checking
  ## - **Compile-Time**: All processing happens at compile-time
  ## - **Self-Contained**: Grammar and parser in a single Nim file
  ##
  ## Notes
  ## -----
  ## - The grammar function must be defined before calling `buildGrammar`
  ## - The function name is passed as an identifier (without quotes or parentheses)
  ## - Complex grammars may increase compilation time
  ## - The generated macro output is shown during compilation for debugging
  result = fmt"""
macro buildGrammarImpl(): untyped =
  let inputGrammar = {createGrammarFunction.strVal}()
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
  return parserCode.parseStmt()
buildGrammarImpl()
""".parseStmt()
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
        rep(`child`)
    of "*": # Zero or more (rep(opt(...))) or similar
      # treestand DSL doesn't have `rep_star`.
      # usually rep(opt(x)) or opt(rep(x))?
      # Let's map to `rep(opt(child))` for now, or check if DSL supports it.
      # npeg *P is zero or more.
      # treestand dsl `rep` is one or more. `opt` is zero or one.
      # rep(opt(x)) matches (x?) + -> x? x? x? -> can match empty?
      # Actually `opt(rep(x))` matches (x+)? -> x... or nothing. Correct.
      # Using `opt(rep(child))` for `*child`.
      result = quote do:
        opt(rep(`child`))
    of "?": # Optional
      result = quote do:
        opt(`child`)
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
      # Tuple or complex expression? In rule context, (a, b) probably shouldn't happen unless inside a macro Call?
      # If user writes (a, b), maybe error or treat as sequence?
      # For now, support single exp.
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
  
  # echo result.repr

macro tsGrammar*(name: static string, body: untyped): untyped =
  ## Define a function, `proc name(): InputGrammar` based on Npeg like syntax.
  ## After the function is defined, buildGrammar(name) is called to build the grammar.
  ## 
  ## Example:
  ## ```nim
  ## import treestand
  ##
  ## tsGrammar "my_lang":
  ##   # Rule Assignment
  ##   program     <- +stmt
  ##
  ##   # Sequence (*) and Choice (|)
  ##   stmt        <- assign * semi
  ##  
  ##   # Repetition
  ##   # +rule  -> One or more
  ##   # *rule  -> Zero or more
  ##   # ?rule  -> Optional
  ##   assign      <- (variable: identifier) * eq * (value: expr) # Named fields by (fld : rule) format
  ##   expr        <- identifier | number | external_token # external_token is a token handled by an external scanner (C function), but not implemented in tsGrammar yet
  ##
  ##   # Lexical Tokens
  ##   # Use token() wrapper for lexical rules
  ##   # String literals and regex patterns are auto-wrapped with str() or patt()
  ##   identifier  <- token(re"\w+")
  ##   number      <- token(re"\\d+")
  ##   eq          <- token("=")
  ##   semi        <- token(";")
  ##
  ##   # Configuration·
  ##   extras      = token(re"\s+")
  ##   # word        = "identifier"
  ##
  ## when isMainModule:
  ##   echo parseMyLang("a = 1; b=a;")
  ## ```
  
  var variables = newNimNode(nnkBracket) # @[...]
  var extraSymbols = newNimNode(nnkBracket)
  var externalTokens = newNimNode(nnkBracket)
  var expectedConflicts = newNimNode(nnkBracket)
  var wordToken = quote do:
    none(string)
  var variablesToInline = newNimNode(nnkBracket)
  var supertypeSymbols = newNimNode(nnkBracket)

  for stmt in body:
    if stmt.kind == nnkInfix and stmt[0].strVal == "<-":
      # Rule Assignment: name <- rule
      let nameNode = stmt[1]
      let ruleNode = stmt[2]
      
      let nameStr = nameNode.strVal
      
      # Check if it's an external token definition
      # indent <- external_token
      if ruleNode.kind == nnkIdent and ruleNode.strVal == "external_token":
        # Create a rule referencing this token name and add to externalTokens
        # usually external tokens are just named symbols in the context of InputGrammar
        # that will be matched by external scanner function.
        # We add `sym(nameStr)` to externalTokens.
        let symNode = quote do:
          sym(`nameStr`)
        externalTokens.add symNode
        
        # We also usually need it in variables so it can be referenced?
        # Actually, if it's in externalTokens, it implicitly exists.
        # But let's check if we need to add a Variable for it.
        # If we rely on valid sym references, maybe not needed in variables list if generator looks disjointly.
        # SAFE OPTION: Add a Variable with a special rule (e.g. blank or just metadata)
        # OR just rely on externalTokens being checked.
        # Let's start by ONLY adding to externalTokens.
      else:
        # Standard Rule
        let transformedRule = transformRule(ruleNode)
        let varConstr = quote do:
          Variable(name: `nameStr`, kind: vtNamed, rule: `transformedRule`)
        variables.add varConstr

    elif stmt.kind == nnkAsgn and stmt[0].kind == nnkIdent:
      # Configuration: field = value (or rule assignment with =)
      # We agreed to use <- for rules, so = is likely config.
      # Check reserved names.
      let field = stmt[0].strVal
      let value = stmt[1]
      
      case field
      of "extras":
        # Expecting a rule or list of rules using `token(...)`
        # If value is bracket, iterate. If single, wrap.
        if value.kind == nnkBracket:
          for child in value:
            extraSymbols.add transformRule(child)
        else:
          extraSymbols.add transformRule(value)
      of "conflicts":
        expectedConflicts = value 
      of "scanner":
        # Placeholder
        discard
      of "inline":
        variablesToInline = value
      of "supertypes":
        supertypeSymbols = value
      of "word":
        # value should be string literal
        wordToken = newCall("some", value)
      else:
         # Treat as error
         error("Unknown configuration property or invalid rule assignment (use <-): " & field, stmt)
         
    elif stmt.kind == nnkAsgn and stmt[0].kind == nnkDotExpr:
       # Legacy/Fallback if user really wants .field = val and it parses?
       # But let's disable to be consistent.
       error("Please use 'field = value' without leading dot for configuration.", stmt)
         
    else:
      # Ignore other statements or error?
      # For now, ignore to be safe against the parsing issue seen earlier if possible,
      # OR better, fix the parsing expectation.
      # The AST issue: `m <- a ^ 1.config = "value"`
      # The previous statement `m <- ...` contained the assignment as a child because of precedence.
      # This means `stmt` here IS `m <- ...`.
      # We need to detect if the rule part effectively "captured" the assignment.
      # This is hard.
      # Alternative: Recommend user use `extras: ...` block or syntax that binds tighter.
      
      # Let's stick to standard handling and rely on separate lines/indentation working correctly 
      # or user fixing the syntax (e.g. parens).
      discard
  
  if externalTokens.len > 0:
    echo "[Treestand] Found external tokens for grammar `" & name & "`: " & externalTokens.repr
    echo "[Treestand] Make sure to define scanner functions for these tokens in the scanner.c file."
  
  let grammarProcName = ident(name)
  result = quote do:
    proc `grammarProcName`*(): InputGrammar =
      InputGrammar(
        name: `name`,
        variables: @`variables`,
        extraSymbols: @`extraSymbols`,
        expectedConflicts: @`expectedConflicts`,
        externalTokens: @`externalTokens`,
        variablesToInline: @`variablesToInline`,
        supertypeSymbols: @`supertypeSymbols`,
        wordToken: `wordToken`
      )
    buildGrammar(`grammarProcName`)
  
  echo "[Treestand] Generated InputGrammar constructor: " & grammarProcName.repr

  when defined(debug):
    echo "Generated InputGrammar:"
    echo result.repr
