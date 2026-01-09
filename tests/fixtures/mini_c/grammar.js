module.exports = grammar({
  name: 'mini_c',
  extras: $ => [
    /\s|\\\r?\n/,
  ],
  conflicts: $ => [
     [$.sized_type_specifier],
  ],
  rules: {
    program: $ => repeat($.declaration),
    declaration: $ => seq(
        $._declaration_specifiers,
        $.declarator,
        ';'
    ),
    _declaration_specifiers: $ => prec.right(seq(
        repeat($.declaration_specifiers),
        field('type', $.type_specifier),
        repeat($.declaration_specifiers),
    )),
    declaration_specifiers: $ => choice($.storage_class_specifier, $.type_qualifier),
    storage_class_specifier: $ => choice('static', 'extern'),
    type_qualifier: $ => choice('const', 'volatile'),
    type_specifier: $ => choice(
        $.sized_type_specifier,
        $.primitive_type,
        $.identifier
    ),
    primitive_type: $ => choice('int', 'char', 'void'),
    sized_type_specifier: $ => seq(
        repeat1(choice('signed', 'unsigned')),
        optional(choice($.primitive_type, prec.dynamic(-1, $.identifier)))
    ),
    declarator: $ => $.identifier,
    identifier: $ => /[a-zA-Z_]\w*/,
  }
});