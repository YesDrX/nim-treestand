import parser

let input = "a b"
var p = newParser(input)
try:
  let tree = p.parse()
  echo "SUCCESS: ", tree
except:
  echo "ERROR: ", getCurrentExceptionMsg()
