module.exports = grammar({
  name: 'repro',

  extras: $ => [
    /\s/,
  ],

  rules: {
    source_file: $ => $.seq_rule,
    
    seq_rule: $ => seq(
      'x',
      $.space,
      'y'
    ),
    
    space: $ => /\s/
  }
});