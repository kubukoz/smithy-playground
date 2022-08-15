// Comma-separated sequence of field, with an optional trailing comma.
function comma_separated_trailing(name, $, field) {
  return seq(field, optional(seq(",", optional($[name]))));
}

module.exports = grammar({
  name: "smithyql",

  extras: ($) => [$.whitespace, $.comment],
  rules: {
    source_file: ($) =>
      optional(
        seq(
          field("use_clause", optional($.use_clause)),
          field("operation_name", $.operation_name),
          field("input", $.struct)
        )
      ),

    use_clause: ($) =>
      seq("use", $.whitespace, "service", $.whitespace, $.qualified_identifier),

    qualified_identifier: ($) =>
      seq($.identifier, repeat(seq(".", $.identifier)), "#", $.identifier),

    operation_name: ($) => $.identifier,

    input_node: ($) =>
      choice($.struct, $.list, $.number, $.string, $.boolean, $.null),

    struct: ($) => seq("{", optional($.fields), "}"),
    list: ($) => seq("[", optional($.list_fields), "]"),

    fields: ($) => comma_separated_trailing("fields", $, $.field),

    field: ($) => seq($.identifier, "=", $.input_node),

    list_fields: ($) =>
      comma_separated_trailing("list_fields", $, $.list_field),

    list_field: ($) => $.input_node,

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    boolean: ($) => choice("true", "false"),
    number: ($) => /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/,
    string: ($) => /"([^"\\]|\\.)*"/,

    null: ($) => "null",

    comment: ($) => token(seq("//", /.*/)),
    whitespace: ($) => /\s+/,
  },
});
//
