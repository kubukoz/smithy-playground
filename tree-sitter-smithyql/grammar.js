// Comma-separated sequence of field, with an optional trailing comma.
function comma_separated_trailing(field_grammar) {
  return prec.left(
    1,
    seq(field_grammar, repeat(seq(",", field_grammar)), optional(","))
  );
}

module.exports = grammar({
  name: "smithyql",

  extras: ($) => [$.whitespace, $.comment],
  rules: {
    source_file: ($) =>
      seq(
        field("use_clause", optional($.use_clause)),
        field("statements", seq($.top_level_statement))
      ),

    top_level_statement: ($) => choice($.let_binding, $.operation_call),

    let_binding: ($) => seq("let", $.whitespace, $.binding, optional(",")),

    operation_call: ($) =>
      seq(field("operation_name", $.operation_name), field("input", $.struct)),

    use_clause: ($) =>
      seq(
        "use",
        $.whitespace,
        "service",
        $.whitespace,
        field("identifier", $.qualified_identifier)
      ),

    qualified_identifier: ($) =>
      seq(
        field("head", $.identifier),
        field("tail", repeat(seq(".", $.identifier))),
        "#",
        field("selection", $.identifier)
      ),

    operation_name: ($) =>
      prec.left(
        1,
        seq(
          field("identifier", optional(seq($.qualified_identifier, "."))),
          field("name", $.identifier)
        )
      ),

    _input_node: ($) =>
      choice($.struct, $.list, $.number, $.string, $.boolean, $.null),

    struct: ($) => seq("{", field("bindings", optional($.bindings)), "}"),
    list: ($) => seq("[", field("list_fields", optional($.list_fields)), "]"),

    bindings: ($) => comma_separated_trailing($.binding),

    binding: ($) =>
      seq(field("key", $.identifier), "=", field("value", $._input_node)),

    list_fields: ($) => comma_separated_trailing($._input_node),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    boolean: ($) => choice("true", "false"),
    number: ($) => /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/,
    string: ($) => /"([^"\\]|\\.)*"/,

    null: ($) => "null",

    comment: ($) => token(seq("//", /.*/)),
    whitespace: ($) => /\s+/,
  },
  supertypes: ($) => [$._input_node],
});
//
