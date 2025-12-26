#include "tree_sitter/parser.h"

enum TokenType {
  EXTERNAL_TOKEN,
};

void *tree_sitter_precedence_repro_external_scanner_create() { return NULL; }
void tree_sitter_precedence_repro_external_scanner_destroy(void *p) {}
unsigned tree_sitter_precedence_repro_external_scanner_serialize(void *p, char *buffer) { return 0; }
void tree_sitter_precedence_repro_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

bool tree_sitter_precedence_repro_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  // If we are at the start of input, return the external token
  if (valid_symbols[EXTERNAL_TOKEN] && !lexer->eof(lexer)) {
    lexer->result_symbol = EXTERNAL_TOKEN;
    lexer->advance(lexer, false);
    return true;
  }
  return false;
}