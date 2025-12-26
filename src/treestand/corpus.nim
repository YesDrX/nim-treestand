## Corpus parser for Tree-sitter test format
##
## Parses corpus.txt files containing test cases in the format:
## ```
## =================================
## Test Name
## =================================
## input code
## ---
## (expected s-expression)
## ```

import std/[strutils]

type
  TestCase* = object
    name*: string
    input*: string
    expected*: string
  
  CorpusParseError* = object of CatchableError



proc parseCorpus*(content: string): seq[TestCase] =
  ## Parse a corpus.txt file into test cases
  var tests: seq[TestCase] = @[]
  var lines = content.split('\n')
  var i = 0
  
  while i < lines.len:
    # Skip empty lines and comments
    while i < lines.len and (lines[i].strip().len == 0 or lines[i].strip().startsWith("#")):
      inc i
    
    if i >= lines.len:
      break
    
    # Look for test separator (==================)
    if not lines[i].strip().startsWith("===="):
      inc i
      continue
    
    inc i  # Skip first separator
    if i >= lines.len:
      break
    
    # Get test name
    let testName = lines[i].strip()
    inc i
    
    if i >= lines.len:
      break
    
    # Skip second separator
    if not lines[i].strip().startsWith("===="):
      inc i
      continue
    inc i
    
    # Collect input lines until separator (---) or next test
    var inputLines: seq[string] = @[]
    while i < lines.len and not lines[i].strip().startsWith("---") and not lines[i].strip().startsWith("===="):
      inputLines.add(lines[i])
      inc i
    
    let input = inputLines.join("\n").strip(leading = true, trailing = false)
    
    # Check if we have expected output
    var expected = ""
    if i < lines.len and lines[i].strip().startsWith("---"):
      inc i  # Skip separator
      
      # Collect expected output lines until next test or EOF
      var expectedLines: seq[string] = @[]
      while i < lines.len and not lines[i].strip().startsWith("===="):
        expectedLines.add(lines[i])
        inc i
      
      expected = expectedLines.join("\n").strip()
    
    # Add test case
    if testName.len > 0 and input.len > 0:
      tests.add(TestCase(
        name: testName,
        input: input,
        expected: expected
      ))
  
  return tests
