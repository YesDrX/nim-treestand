## Execute JavaScript grammar files to get JSON representation

import os, osproc, strutils, options

type
  JsExecError* = object of CatchableError

proc findExeStatic*(name: string): string {.compileTime.} =
  when defined(windows):
    for ext in ["", ".exe", ".cmd"]:
      let (outp, err) = gorgeEx("where " & name & ext)
      if err == 0 and outp.len > 0:
        return outp.strip().splitLines()[0]
  let (outp, err) = gorgeEx("which " & name)
  if err == 0 and outp.len > 0:
    return outp.strip()
  return ""

proc findExeEx*(name: string): string =
  when nimvm:
    let found = findExeStatic(name)
    if found.len > 0:
      return found
  else:
    let foundRuntime = findExe(name)
    if foundRuntime.len > 0:
      return foundRuntime
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