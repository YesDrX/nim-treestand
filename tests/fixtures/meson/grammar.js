module.exports = grammar({
  name: "meson",

  extras: ($) => [
    /\s/,
    /\\\n/, // escaped newline, treat as whitespace
    $.comment,
    /\n/,
  ],

  inline: ($) => [],

  conflicts: ($) => [[$._positional_arguments]],

  precedences: (_$) => [
    ["multiply", "add", "compare", "equality", "logical_and", "logical_or"],
  ],

  word: ($) => $.identifier,

  rules: {
    build_definition: ($) => repeat(choice("\n", $.statement)),

    // comments
    comment: (_$) => seq("#", /[^\n]*/, "\n"),

    // keywords
    if: (_$) => token("if"),
    elif: (_$) => token("elif"),
    else: (_$) => token("else"),
    endif: (_$) => token("endif"),
    foreach: (_$) => token("foreach"),
    endforeach: (_$) => token("endforeach"),
    break: (_$) => token("break"),
    continue: (_$) => token("continue"),
    and: (_$) => token("and"),
    or: (_$) => token("or"),
    not: (_$) => token("not"),
    in: (_$) => token("in"),

    // basic types
    boolean_literal: (_$) => token(choice("true", "false")),

    int_literal: (_$) =>
      token(
        choice(
          /0[Xx][a-fA-F0-9]+/, // hex
          /[1-9][0-9]*/, //decimal (not zero)
          /0[Oo][0-7]+/, // octal
          /0[Bb][01]+/, // binary
          "0" // zero
        )
      ),

    string_literal: ($) =>
      choice(
        seq(
          "'",
          repeat(choice($.escape_sequence, /[^'\\]/, $.bad_escape)),
          "'"
        ),
        seq("'''", repeat(/./), "'''")
      ),

    // formatted strings - we don't match out the parameters
    // TODO: add support for calling out format parameters
    fstring_literal: ($) =>
      choice(
        seq(
          "f'",
          repeat(choice($.escape_sequence, /[^'\\]/, $.bad_escape)),
          "'"
        ),
        seq("f'''", repeat(/./), "'''")
      ),

    // known supported escape sequences
    escape_sequence: (_$) =>
      prec(
        1,
        choice(
          "\\\\",
          "\\'",
          "\\a",
          "\\b",
          "\\f",
          "\\n",
          "\\r",
          "\\t",
          "\\v",
          /\\x[0-9A-Fa-f]{2}/,
          /\\u[0-9A-Fa-f]{4}/,
          /\\U[0-9A-Fa-f]{8}/,
          /\\[0-7]{1,3}/, // octal
          /\\N\{[A-Za-z- ]+\}/ // unicode named characters
        )
      ),

    // we call these out so that they can be marked invalid
    bad_escape: (_$) => prec(-1, /\\[^\\'abfnrtvxuUN\n]/),

    array_literal: ($) => seq("[", optional($._expression_list), "]"),

    _expression_list: ($) => seq($.expression, repeat(seq(",", $.expression))),

    dictionary_literal: ($) => seq("{", optional($._key_value_list), "}"),

    key_value_item: ($) =>
      seq(field("key", $.expression), ":", field("value", $.expression)),

    _key_value_list: ($) =>
      seq($.key_value_item, repeat(seq(",", $.key_value_item))),

    _literal: ($) =>
      choice(
        $.boolean_literal,
        $.int_literal,
        $.string_literal,
        $.fstring_literal,
        $.array_literal,
        $.dictionary_literal
      ),

    identifier: (_$) => /[a-zA-Z_][a-zA-Z_0-9]*/,

    statement: ($) =>
      seq(
        choice(
          $.expression_statement,
          $.selection_statement,
          $.iteration_statement,
          $.assignment_statement,
          $.break,
          $.continue
        ),
        "\n"
      ),

    expression_statement: ($) => $.expression,

    expression: ($) =>
      choice(
        $.conditional_expression,
        $.logical_or_expression,
        $.logical_and_expression,
        $.equality_expression,
        $.relational_expression,
        $.additive_expression,
        $.multiplicative_expression,
        $.unary_expression,
        $.subscript_expression,
        $.primary_expression,
        $.function_expression,
        $.method_expression,
        $._literal,
        $.identifier
      ),

    assignment_statement: ($) =>
      seq($.expression, $.assignment_operator, $.expression),

    assignment_operator: (_$) => choice("=", "*=", "/=", "%=", "+=", "-="),

    conditional_expression: ($) =>
      prec.right(
        seq(
          field("condition", $.expression),
          "?",
          field("consequence", $.expression),
          ":",
          field("alternative", $.expression)
        )
      ),

    logical_or_expression: ($) =>
      prec.left("logical_or", seq($.expression, $.or, $.expression)),

    logical_and_expression: ($) =>
      prec.left("logical_and", seq($.expression, $.and, $.expression)),

    equality_operator: (_$) => choice("==", "!="),

    equality_expression: ($) =>
      prec.left(
        "equality",
        seq($.expression, $.equality_operator, $.expression)
      ),

    relational_operator: ($) =>
      choice(">", "<", ">=", "<=", $.in, seq($.not, $.in)),

    relational_expression: ($) =>
      prec.left(
        "compare",
        seq($.expression, $.relational_operator, $.expression)
      ),

    additive_operator: (_$) => choice("+", "-"),

    additive_expression: ($) =>
      prec.left("add", seq($.expression, $.additive_operator, $.expression)),

    multiplicative_operator: (_$) => choice("*", "/", "%"),

    multiplicative_expression: ($) =>
      prec.left(
        "multiply",
        seq($.expression, $.multiplicative_operator, $.expression)
      ),

    unary_operator: ($) => choice($.not, "+", "-"),

    unary_expression: ($) => prec.right(seq($.unary_operator, $.expression)),

    primary_expression: ($) => seq("(", $.expression, ")"),

    subscript_expression: ($) => seq($.expression, "[", $.expression, "]"),

    function_expression: ($) =>
      seq(
        field("function", $.identifier),
        "(",
        optional($._argument_list),
        ")"
      ),

    _argument_list: ($) =>
      choice(
        seq($._positional_arguments, optional(seq(",", $._keyword_arguments))),
        $._keyword_arguments
      ),

    _positional_arguments: ($) => seq($.argument, repeat(seq(",", $.argument))),

    argument: ($) => $.expression,

    _keyword_arguments: ($) =>
      seq($.keyword_argument, repeat(seq(",", $.keyword_argument))),

    keyword_argument: ($) =>
      seq(field("keyword", $.identifier), ":", $.expression),

    method_expression: ($) =>
      prec.left(seq($.expression, ".", $.function_expression)),

    // if clauses can have a jump if they occur in a loop.
    // However, it is nonsensical for those clauses to occur
    // at any location other than the last statement in the branch.
    selection_statement: ($) =>
      prec.right(
        seq(
          $.if,
          $.condition,
          "\n",
          optional($.statement_list),
          repeat(seq($.elif, $.condition, "\n", optional($.statement_list))),
          optional(seq($.else, optional($.statement_list))),
          $.endif
        )
      ),

    statement_list: ($) => repeat1($.statement),

    condition: ($) => $.expression,

    iteration_statement: ($) =>
      seq(
        $.foreach,
        $.identifier_list,
        ":",
        $.identifier,
        "\n",
        optional($.statement_list),
        $.endforeach
      ),

    identifier_list: ($) => seq($.identifier, repeat(seq(",", $.identifier))),
  },
});