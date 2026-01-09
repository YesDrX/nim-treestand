module.exports = grammar({
  name: 'conflict_repro',

  conflicts: $ => [
    [$.reduce_rule, $.shift_rule]
  ],

  rules: {
    source: $ => repeat($._item),

    _item: $ => choice(
       seq($.reduce_rule, 'b'),
       $.shift_rule
    ),

    reduce_rule: $ => 'a',
    shift_rule: $ => seq('a', 'b')
  }
});
