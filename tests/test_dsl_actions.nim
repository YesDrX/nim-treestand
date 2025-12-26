
import std/[unittest, strutils, tables, options]
import treestand

# Define User Data Structure
type
  Request = object
    proto: string
    version: string
    code: int
    message: string
    headers: Table[string, string]

# Define Grammar with Actions
tsGrammar "http", userdata: Request:
  # Entry point
  http        <- response * crlf * ?headers * eof

  # Rules
  response    <- proto * slash * version * space * code * space * msg 
  
  # Actions
  proto       <- token(re"[a-zA-Z]+"):
      userdata.proto = $1
      
  version     <- token(re"\d+\.\d+"):
      userdata.version = $1
      
  code        <- token(re"\d+"):
      userdata.code = parseInt($1)
      
  msg         <- token(re"[^\r\n]+"):
      userdata.message = $1

  # Header handling
  # header: name * ": " * val
  # $1 = name, $2 = ": ", $3 = val
  # We use token for name and val to ensure they are captured as single nodes
  header      <- header_name * colon_sp * header_val:
      userdata.headers[$1] = $3

  headers     <- +(header * crlf)
  
  # Lexical tokens
  header_name <- token(re"[a-zA-Z\-]+")
  header_val  <- token(re"[^\r\n]+")
  colon_sp    <- token(": ")
  slash       <- token("/")
  space       <- token(" ")
  crlf        <- token(re"\r?\n")
  eof         <- token(re"") # tree-sitter handling of EOF? usually explicit? 
                             # tsGrammar doesn't need explicit EOF usually, root rule covers input.
                             # If we want to ensure full match, we rely on parser.
                             # Let's remove explicit EOF token for now as it might be tricky.
  
  # We don't need explicit EOF rule if we match the whole input.

# Test logic
test "HTTP Parser with Actions":
  var req: Request
  let data = "HTTP/1.1 301 Moved Permanently\r\nContent-Length: 162\r\nContent-Type: text/html\r\nLocation: https://nim.org/\r\n"
  
  # Initialize headers
  req.headers = initTable[string, string]()
  
  let ok = matchHttp(data, req)
  
  if not ok:
    echo "Parsing failed"
    fail()
  
  check req.proto == "HTTP"
  check req.version == "1.1"
  check req.code == 301
  check req.message == "Moved Permanently"
  check req.headers["Content-Length"] == "162"
  check req.headers["Content-Type"] == "text/html"
  check req.headers["Location"] == "https://nim.org/"
