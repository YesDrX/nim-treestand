import std/[os, osproc, strformat, strutils, logging]

# Configuration
const rootDir = currentSourcePath().parentDir().parentDir()
const DocsDir = rootDir / "docs"
const SrcDir = rootDir / "src"
const ProjectFile = SrcDir / "treestand.nim"
const RepoUrl = "https://github.com/YesDrX/nim-treestand"
const Commit = "master"

var logger = newConsoleLogger()
addHandler(logger)

proc run(cmd: string) =
  info "Running: ", cmd
  let res = execCmd(cmd)
  if res != 0:
    error "Command failed with exit code ", res, ": ", cmd
    quit(res)

proc main() =
  # Ensure docs directory exists
  createDir(DocsDir)

  # 1. Generate API Documentation
  info "Generatiing API Documentation..."
  # doc --project generates docs for the whole package
  # --index:on creates theindex.html
  let apiCmd = fmt"nim doc --project --index:on --git.url:{RepoUrl} --git.commit:{Commit} --outdir:{DocsDir} {ProjectFile}"
  run(apiCmd)

  # 2. Convert Markdown Guides to HTML
  info "Converting Guides..."
  for file in walkFiles(DocsDir / "*.md"):
    let (_, name, _) = splitFile(file)
    # Skip if it's already generated or if it's special (though usually we want to process all .md)
    let cmd = fmt"nim md2html --outdir:{DocsDir} {file}"
    run(cmd)

  # 3. Handle Index Page
  # If we have an index.md, nim doc will convert it to index.html
  # We might need to rename it if it conflicts or if we want it to be the main page
  # The apiCmd generated src/treestand.html and theindex.html
  
  # If docs/index.md exists, compiling it produces docs/index.html
  if fileExists(DocsDir / "index.md"):
    info "Generating Index Page..."
    run(fmt"nim md2html --outdir:{DocsDir} {DocsDir}/index.md")
  
  info "Documentation generated successfully in ", DocsDir

when isMainModule:
  main()
