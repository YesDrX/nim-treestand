module.exports = grammar({
  name: 'reduce_repro',

  rules: {
    source: $ => choice(
      $.rule_a,
      $.rule_b
    ),

    // Both rules match the exact same token 'word'
    // This creates a classic Reduce/Reduce conflict.
    // We add dynamic precedence to force them to be kept as distinct items 
    // but with potentially colliding actions if not deduplicated correctly.
    
    rule_a: $ => prec.dynamic(1, 'word'),
    rule_b: $ => prec.dynamic(2, 'word'),
  }
});