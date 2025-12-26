module.exports = grammar({
  name: 'sexp',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    program: $ => repeat($.expression),

    expression: $ => choice(
      $.list,
      $.atom,
      $.string,
      $.wildcard,
      $.capture,
      $.field,
      $.anchor
    ),

    list: $ => seq(
      '(',
      repeat($.expression),
      ')'
    ),

    // Identifiers: allow alphanumeric, underscore, hyphen, dot? 
    // Tree-sitter node names are usually lower_snake_case.
    atom: $ => /[a-zA-Z_][a-zA-Z0-9_\-\.]*/,

    string: $ => seq(
      '"',
      repeat(choice(
        /[^"\\\n]+/,
        /\\./
      )),
      '"'
    ),

    wildcard: $ => '_',

    // @capture_name
    capture: $ => /@[a-zA-Z0-9_\-\.]+/,

    // field_name:
    field: $ => /[a-zA-Z0-9_\-]+:/,

    // Anchor .
    anchor: $ => '.',
    
    comment: $ => /;.*/,
  }
});
