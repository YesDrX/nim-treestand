module.exports = grammar({
  name: 'example',

  externals: $ => [
    $.comment
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.expression,
      $.comment
    )),

    expression: $ => choice(
      $.number,
      $.binary_op
    ),

    binary_op: $ => prec.left(1, seq(
      $.expression,
      '+',
      $.expression
    )),

    number: $ => /\d+/
  }
});
