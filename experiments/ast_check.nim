
import macros

macro printAst(body: untyped): untyped =
  echo body.treeRepr
  
printAst:
  program <- expression:
     echo $1
  
  other <- foo
