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
        field("prelude", optional($.prelude)),
        field("statements", repeat($.top_level_statement))
      ),

    prelude: ($) => repeat1($.use_clause),

    // todo: use token.immediate to prevent comments?
    // or just allow comments everywhere?
    use_clause: ($) =>
      seq(
        "use",
        $.whitespace,
        "service",
        $.whitespace,
        field("identifier", $.qualified_identifier)
      ),

    top_level_statement: ($) => choice($.run_query),

    run_query: ($) =>
      seq(
        field("operation_name", $.query_operation_name),
        field("input", $.struct)
      ),

    _namespace: ($) => seq($.identifier, repeat(seq(".", $.identifier))),

    qualified_identifier: ($) =>
      seq(
        field("namespace", $._namespace),
        "#",
        field("selection", $.identifier)
      ),

    operation_name_qualifier: ($) => seq($.qualified_identifier, "."),

    // todo: model as union?
    query_operation_name: ($) =>
      seq(
        field("identifier", optional($.operation_name_qualifier)),
        field("name", $.operation_name)
      ),

    operation_name: ($) => $.identifier,

    _input_node: ($) =>
      choice($.struct, $.list, $.number, $.string, $.boolean, $.null),

    struct: ($) => seq("{", field("bindings", optional($._bindings)), "}"),
    list: ($) => seq("[", field("list_fields", optional($._list_fields)), "]"),

    _bindings: ($) => comma_separated_trailing($.binding),

    binding: ($) =>
      seq(
        field("key", $.identifier),
        choice("=", ":"),
        field("value", $._input_node)
      ),

    _list_fields: ($) => comma_separated_trailing($._input_node),

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
