#include "tree_sitter/parser.h"
#include <ctype.h>
#include <stdio.h>

// Defines from grammar.js
// externals: [$.comment]
// So valid_symbols[0] = comment

enum TokenType {
  COMMENT = 0
};

void *tree_sitter_example_external_scanner_create() {
  return NULL;
}

void tree_sitter_example_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_example_external_scanner_serialize(
  void *payload,
  char *buffer
) {
  return 0;
}

void tree_sitter_example_external_scanner_deserialize(
  void *payload,
  const char *buffer,
  unsigned length
) {
}

bool tree_sitter_example_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  // If expecting comment (COMMENT symbol at index 0 of externals)
  if (valid_symbols[COMMENT]) {
    
    // Skip whitespace
    while (isspace(lexer->lookahead)) {
      lexer->advance(lexer, true);
    }
    
    // Check for comment start '#'
    if (lexer->lookahead == '#') {
      lexer->advance(lexer, false);
      
      // Consume until newline or EOF
      while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
        lexer->advance(lexer, false);
      }
      
      lexer->result_symbol = COMMENT;
      lexer->mark_end(lexer);
      return true;
    }
  }
  
  return false;
}
