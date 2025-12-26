// Grammar with multiple external tokens and named fields
module.exports = grammar({
  name: 'simple_scanner',

  externals: $ => [
    $._indent,
    $._dedent,
    $._newline
  ],

  rules: {
    source_file: $ => repeat($.statement),

    statement: $ => choice(
      $.assignment,
      $.print_statement,
      $.if_statement
    ),

    assignment: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $.expression),
      $._newline
    ),

    print_statement: $ => seq(
      'print',
      field('argument', $.expression),
      $._newline
    ),

    if_statement: $ => seq(
      'if',
      field('condition', $.expression),
      ':',
      $._newline,
      field('body', $.block)
    ),

    block: $ => seq(
      $._indent,
      repeat1($.statement),
      $._dedent
    ),

    expression: $ => choice(
      $.identifier,
      $.number,
      $.binary_expression
    ),

    binary_expression: $ => prec.left(1, seq(
      field('left', $.expression),
      field('operator', choice('+', '-', '*', '/')),
      field('right', $.expression)
    )),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    number: $ => /[0-9]+/
  }
});
