module.exports = grammar({
  name: 'precedence_repro',

  externals: $ => [
    $.external_token // We use an external to match your specific scanner logic path
  ],

  rules: {
    // We explicitly chose two paths.
    // Both paths lead to `shared_rule`, but apply different precedence.
    source: $ => choice(
      $.path_a,
      $.path_b
    ),

    // Path A imposes precedence 1 on shared_rule
    path_a: $ => prec(1, $.shared_rule),

    // Path B imposes precedence 2 on shared_rule
    path_b: $ => prec(2, $.shared_rule),

    // The shared rule matches a single token
    // This creates two Items in the final state:
    // 1. shared_rule -> external_token . (Inherited Prec: 1)
    // 2. shared_rule -> external_token . (Inherited Prec: 2)
    // Both dictate: REDUCE(shared_rule)
    shared_rule: $ => $.external_token
  }
});