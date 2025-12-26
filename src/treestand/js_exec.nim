## Execute JavaScript grammar files to get JSON representation

import os, osproc, strutils, options, strformat

type
  JsExecError* = object of CatchableError

proc findExeStatic*(name: string): string =
  when defined(windows):
    let (output, exitcode) = gorgeEx("where " & name & ".exe")
  else:
    let (output, exitcode) = gorgeEx("which " & name)
  
  if exitcode == 0 and output.len > 0:
    return output.strip()

  let (output2, exitcode2) = gorgeEx(fmt"""ls {"~/.nimble/bin/".expandTilde() / name}""")
  if exitcode2 == 0 and output2.len > 0:
    return "~/.nimble/bin/".expandTilde() / name

  return ""

proc findExeEx*(name: string): string =
  if findExeStatic(name).len > 0:
    return name
  if findExe(name).len > 0:
    return name
  return ""

proc findJsRuntime*(): Option[string] =
  ## Find bun or nodejs executable using findExe
  let bun = findExeEx("bun")
  if bun.len > 0:
    return some(bun)

  let deno = findExeEx("deno")
  if deno.len > 0:
    return some(deno)
  
  let node = findExeEx("node")
  if node.len > 0:
    return some(node)
  
  let nodejs = findExeEx("nodejs")
  if nodejs.len > 0:
    return some(nodejs)
  
  return none(string)

proc executeGrammarJs*(grammarPath: string, dslScriptPath: string = ""): string =
  ## Execute a grammar.js file and return the JSON output
  let runtime = findJsRuntime()
  if runtime.isNone:
    raise newException(JsExecError, "No JavaScript runtime found (bun, node, or nodejs)")
  
  let runtimePath = runtime.get()
  
  var dslPath = dslScriptPath
  if dslPath.len == 0:
    dslPath = currentSourcePath().parentDir() / "dsl.js"
    
  if not fileExists(dslPath):
    raise newException(JsExecError, "dsl.js not found at: " & dslPath)
  
  if not fileExists(grammarPath):
    raise newException(JsExecError, "Grammar file not found: " & grammarPath)
  
  # Set environment variable for grammar path
  # Use putEnv to set the environment variable before execution
  let oldEnv = getEnv("TREE_SITTER_GRAMMAR_PATH", "")
  putEnv("TREE_SITTER_GRAMMAR_PATH", grammarPath)
  
  # Execute: runtime dsl.js
  # The dsl.js will import the grammar and output JSON
  # For bun, we need to use --bun flag or just run directly
  # For node, we might need --input-type=module or use .mjs
  let cmd = runtimePath & " " & dslPath.quoteShell
  let (output, exitCode) = execCmdEx(cmd, options = {poStdErrToStdOut})
  
  # Restore old environment variable
  if oldEnv.len > 0:
    putEnv("TREE_SITTER_GRAMMAR_PATH", oldEnv)
  else:
    delEnv("TREE_SITTER_GRAMMAR_PATH")
  
  if exitCode != 0:
    raise newException(JsExecError, "Failed to execute grammar.js: " & output)
  
  output.strip()

proc executeGrammarJsStatic*(grammarPath: string, dslScriptPath: string = ""): string =
  let runtime = findJsRuntime()
  if runtime.isNone:
    raise newException(JsExecError, "No JavaScript runtime found (bun, node, or nodejs)")
  
  let runtimePath = runtime.get()
  echo "[Treestand] Using JavaScript runtime: " & runtimePath

  var dslPath = dslScriptPath
  if dslPath.len == 0:
    dslPath = currentSourcePath().parentDir() / "dsl.js"

  let oldEnv = getEnv("TREE_SITTER_GRAMMAR_PATH", "")
  putEnv("TREE_SITTER_GRAMMAR_PATH", grammarPath)

  let cmd = runtimePath & " " & dslPath.quoteShell
  let (output, exitCode) = gorgeEx(cmd)
  
  if oldEnv.len > 0:
    putEnv("TREE_SITTER_GRAMMAR_PATH", oldEnv)
  else:
    delEnv("TREE_SITTER_GRAMMAR_PATH")

  if exitCode != 0:
    raise newException(JsExecError, "Failed to execute grammar.js:\n" & output)

  output.strip()