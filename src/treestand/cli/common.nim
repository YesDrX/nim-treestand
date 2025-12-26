import std/[os]

proc findDslJs*(): string =
  ## Find dsl.js in standard locations
  # 1. Check environment variable
  let envPath = getEnv("TREE_SITTER_DSL_PATH")
  if envPath.len > 0 and fileExists(envPath):
    return envPath
  
  # 2. Check relative to executable
  let relPath = currentSourcePath().parentDir().parentDir() / "dsl.js"
  if fileExists(relPath):
    return relPath

  raise newException(IOError, "Could not find dsl.js. Set TREE_SITTER_DSL_PATH or use --dsl option.")

