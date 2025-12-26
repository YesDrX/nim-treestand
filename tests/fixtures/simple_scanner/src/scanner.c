#include "tree_sitter/parser.h"
#include <string.h>
#include <wctype.h>

enum TokenType {
  INDENT,
  DEDENT,
  NEWLINE
};

typedef struct {
  int indent_level;
  int *indent_stack;
  int stack_size;
  int stack_capacity;
} Scanner;

void *tree_sitter_simple_scanner_external_scanner_create() {
  Scanner *scanner = (Scanner *)malloc(sizeof(Scanner));
  scanner->indent_level = 0;
  scanner->stack_capacity = 8;
  scanner->stack_size = 1;
  scanner->indent_stack = (int *)malloc(sizeof(int) * scanner->stack_capacity);
  scanner->indent_stack[0] = 0;  // Base indentation
  return scanner;
}

void tree_sitter_simple_scanner_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  if (scanner->indent_stack) {
    free(scanner->indent_stack);
  }
  free(scanner);
}

unsigned tree_sitter_simple_scanner_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  if (scanner->stack_size > 0) {
    int size = scanner->stack_size * sizeof(int);
    if (size > 256) size = 256;  // Limit buffer size
    memcpy(buffer, scanner->indent_stack, size);
    return size;
  }
  return 0;
}

void tree_sitter_simple_scanner_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  if (length > 0) {
    scanner->stack_size = length / sizeof(int);
    if (scanner->stack_size > scanner->stack_capacity) {
      scanner->indent_stack = (int *)realloc(scanner->indent_stack, scanner->stack_size * sizeof(int));
      scanner->stack_capacity = scanner->stack_size;
    }
    memcpy(scanner->indent_stack, buffer, length);
  }
}

bool tree_sitter_simple_scanner_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;
  
  // Simple implementation: recognize newlines and track indentation
  if (valid_symbols[NEWLINE]) {
    if (lexer->lookahead == '\n' || lexer->lookahead == '\r') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '\n') {
        lexer->advance(lexer, false);
      }
      lexer->result_symbol = NEWLINE;
      return true;
    }
  }
  
  // Placeholder indent/dedent logic - just return false for now
  // Real implementation would count spaces and track indent stack
  
  return false;
}
