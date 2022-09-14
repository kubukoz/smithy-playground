#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 58
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 37
#define ALIAS_COUNT 0
#define TOKEN_COUNT 20
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 13
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 12

enum {
  anon_sym_let = 1,
  anon_sym_COMMA = 2,
  anon_sym_use = 3,
  anon_sym_service = 4,
  anon_sym_DOT = 5,
  anon_sym_POUND = 6,
  anon_sym_LBRACE = 7,
  anon_sym_RBRACE = 8,
  anon_sym_LBRACK = 9,
  anon_sym_RBRACK = 10,
  anon_sym_EQ = 11,
  sym_identifier = 12,
  anon_sym_true = 13,
  anon_sym_false = 14,
  sym_number = 15,
  sym_string = 16,
  sym_null = 17,
  sym_comment = 18,
  sym_whitespace = 19,
  sym_source_file = 20,
  sym_top_level_statement = 21,
  sym_let_binding = 22,
  sym_operation_call = 23,
  sym_use_clause = 24,
  sym_qualified_identifier = 25,
  sym_operation_name = 26,
  sym_input_node = 27,
  sym_struct = 28,
  sym_list = 29,
  sym_bindings = 30,
  sym_binding = 31,
  sym_list_fields = 32,
  sym_boolean = 33,
  aux_sym_qualified_identifier_repeat1 = 34,
  aux_sym_bindings_repeat1 = 35,
  aux_sym_list_fields_repeat1 = 36,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_let] = "let",
  [anon_sym_COMMA] = ",",
  [anon_sym_use] = "use",
  [anon_sym_service] = "service",
  [anon_sym_DOT] = ".",
  [anon_sym_POUND] = "#",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_EQ] = "=",
  [sym_identifier] = "identifier",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [sym_number] = "number",
  [sym_string] = "string",
  [sym_null] = "null",
  [sym_comment] = "comment",
  [sym_whitespace] = "whitespace",
  [sym_source_file] = "source_file",
  [sym_top_level_statement] = "top_level_statement",
  [sym_let_binding] = "let_binding",
  [sym_operation_call] = "operation_call",
  [sym_use_clause] = "use_clause",
  [sym_qualified_identifier] = "qualified_identifier",
  [sym_operation_name] = "operation_name",
  [sym_input_node] = "input_node",
  [sym_struct] = "struct",
  [sym_list] = "list",
  [sym_bindings] = "bindings",
  [sym_binding] = "binding",
  [sym_list_fields] = "list_fields",
  [sym_boolean] = "boolean",
  [aux_sym_qualified_identifier_repeat1] = "qualified_identifier_repeat1",
  [aux_sym_bindings_repeat1] = "bindings_repeat1",
  [aux_sym_list_fields_repeat1] = "list_fields_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_use] = anon_sym_use,
  [anon_sym_service] = anon_sym_service,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_POUND] = anon_sym_POUND,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_EQ] = anon_sym_EQ,
  [sym_identifier] = sym_identifier,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [sym_number] = sym_number,
  [sym_string] = sym_string,
  [sym_null] = sym_null,
  [sym_comment] = sym_comment,
  [sym_whitespace] = sym_whitespace,
  [sym_source_file] = sym_source_file,
  [sym_top_level_statement] = sym_top_level_statement,
  [sym_let_binding] = sym_let_binding,
  [sym_operation_call] = sym_operation_call,
  [sym_use_clause] = sym_use_clause,
  [sym_qualified_identifier] = sym_qualified_identifier,
  [sym_operation_name] = sym_operation_name,
  [sym_input_node] = sym_input_node,
  [sym_struct] = sym_struct,
  [sym_list] = sym_list,
  [sym_bindings] = sym_bindings,
  [sym_binding] = sym_binding,
  [sym_list_fields] = sym_list_fields,
  [sym_boolean] = sym_boolean,
  [aux_sym_qualified_identifier_repeat1] = aux_sym_qualified_identifier_repeat1,
  [aux_sym_bindings_repeat1] = aux_sym_bindings_repeat1,
  [aux_sym_list_fields_repeat1] = aux_sym_list_fields_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_use] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_service] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_POUND] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [sym_number] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_null] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_whitespace] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_top_level_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_let_binding] = {
    .visible = true,
    .named = true,
  },
  [sym_operation_call] = {
    .visible = true,
    .named = true,
  },
  [sym_use_clause] = {
    .visible = true,
    .named = true,
  },
  [sym_qualified_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_operation_name] = {
    .visible = true,
    .named = true,
  },
  [sym_input_node] = {
    .visible = true,
    .named = true,
  },
  [sym_struct] = {
    .visible = true,
    .named = true,
  },
  [sym_list] = {
    .visible = true,
    .named = true,
  },
  [sym_bindings] = {
    .visible = true,
    .named = true,
  },
  [sym_binding] = {
    .visible = true,
    .named = true,
  },
  [sym_list_fields] = {
    .visible = true,
    .named = true,
  },
  [sym_boolean] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_qualified_identifier_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_bindings_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_list_fields_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_bindings = 1,
  field_head = 2,
  field_identifier = 3,
  field_input = 4,
  field_key = 5,
  field_list_fields = 6,
  field_name = 7,
  field_operation_name = 8,
  field_selection = 9,
  field_statements = 10,
  field_tail = 11,
  field_use_clause = 12,
  field_value = 13,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_bindings] = "bindings",
  [field_head] = "head",
  [field_identifier] = "identifier",
  [field_input] = "input",
  [field_key] = "key",
  [field_list_fields] = "list_fields",
  [field_name] = "name",
  [field_operation_name] = "operation_name",
  [field_selection] = "selection",
  [field_statements] = "statements",
  [field_tail] = "tail",
  [field_use_clause] = "use_clause",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 1},
  [3] = {.index = 2, .length = 2},
  [4] = {.index = 4, .length = 2},
  [5] = {.index = 6, .length = 2},
  [6] = {.index = 8, .length = 3},
  [7] = {.index = 11, .length = 3},
  [8] = {.index = 14, .length = 1},
  [9] = {.index = 15, .length = 2},
  [10] = {.index = 17, .length = 1},
  [11] = {.index = 18, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_statements, 0},
  [2] =
    {field_statements, 1},
    {field_use_clause, 0},
  [4] =
    {field_input, 1},
    {field_operation_name, 0},
  [6] =
    {field_head, 0},
    {field_selection, 2},
  [8] =
    {field_identifier, 0},
    {field_identifier, 1},
    {field_name, 2},
  [11] =
    {field_head, 0},
    {field_selection, 3},
    {field_tail, 1},
  [14] =
    {field_bindings, 1},
  [15] =
    {field_key, 0},
    {field_value, 2},
  [17] =
    {field_identifier, 4},
  [18] =
    {field_list_fields, 1},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(28);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '#') ADVANCE(35);
      if (lookahead == ',') ADVANCE(30);
      if (lookahead == '-') ADVANCE(7);
      if (lookahead == '.') ADVANCE(34);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == '0') ADVANCE(66);
      if (lookahead == '=') ADVANCE(40);
      if (lookahead == '[') ADVANCE(38);
      if (lookahead == ']') ADVANCE(39);
      if (lookahead == 'f') ADVANCE(41);
      if (lookahead == 'l') ADVANCE(43);
      if (lookahead == 'n') ADVANCE(58);
      if (lookahead == 's') ADVANCE(48);
      if (lookahead == 't') ADVANCE(54);
      if (lookahead == 'u') ADVANCE(55);
      if (lookahead == '{') ADVANCE(36);
      if (lookahead == '}') ADVANCE(37);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(67);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '-') ADVANCE(7);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == '0') ADVANCE(66);
      if (lookahead == '[') ADVANCE(38);
      if (lookahead == ']') ADVANCE(39);
      if (lookahead == 'f') ADVANCE(8);
      if (lookahead == 'n') ADVANCE(22);
      if (lookahead == 's') ADVANCE(13);
      if (lookahead == 't') ADVANCE(19);
      if (lookahead == '{') ADVANCE(36);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(67);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(70);
      if (lookahead == '\\') ADVANCE(27);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (lookahead == '.') ADVANCE(34);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == 'l') ADVANCE(43);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 4:
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == 'l') ADVANCE(43);
      if (lookahead == 'u') ADVANCE(55);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 5:
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == '}') ADVANCE(37);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 6:
      if (lookahead == '/') ADVANCE(73);
      END_STATE();
    case 7:
      if (lookahead == '0') ADVANCE(66);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(67);
      END_STATE();
    case 8:
      if (lookahead == 'a') ADVANCE(15);
      END_STATE();
    case 9:
      if (lookahead == 'c') ADVANCE(12);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(62);
      END_STATE();
    case 11:
      if (lookahead == 'e') ADVANCE(64);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(32);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(18);
      END_STATE();
    case 14:
      if (lookahead == 'i') ADVANCE(9);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(20);
      END_STATE();
    case 16:
      if (lookahead == 'l') ADVANCE(71);
      END_STATE();
    case 17:
      if (lookahead == 'l') ADVANCE(16);
      END_STATE();
    case 18:
      if (lookahead == 'r') ADVANCE(23);
      END_STATE();
    case 19:
      if (lookahead == 'r') ADVANCE(21);
      END_STATE();
    case 20:
      if (lookahead == 's') ADVANCE(11);
      END_STATE();
    case 21:
      if (lookahead == 'u') ADVANCE(10);
      END_STATE();
    case 22:
      if (lookahead == 'u') ADVANCE(17);
      END_STATE();
    case 23:
      if (lookahead == 'v') ADVANCE(14);
      END_STATE();
    case 24:
      if (lookahead == '+' ||
          lookahead == '-') ADVANCE(26);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(69);
      END_STATE();
    case 25:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(68);
      END_STATE();
    case 26:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(69);
      END_STATE();
    case 27:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(2);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_use);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_service);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_service);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_POUND);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(52);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(47);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(57);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(31);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(33);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(42);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(50);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(60);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(59);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(46);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(29);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(51);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(45);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'v') ADVANCE(49);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(25);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(24);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(25);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(24);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(67);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(24);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(68);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(69);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_null);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_null);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(61);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(73);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_whitespace);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(74);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 4},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 1},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 3},
  [8] = {.lex_state = 5},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 5},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 5},
  [24] = {.lex_state = 3},
  [25] = {.lex_state = 3},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 5},
  [32] = {.lex_state = 5},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 5},
  [35] = {.lex_state = 3},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 5},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 5},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 5},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 5},
  [50] = {.lex_state = 1},
  [51] = {.lex_state = 0},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 0},
  [54] = {.lex_state = 0},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 0},
  [57] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_use] = ACTIONS(1),
    [anon_sym_service] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_POUND] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [sym_number] = ACTIONS(1),
    [sym_string] = ACTIONS(1),
    [sym_null] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [sym_whitespace] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(54),
    [sym_top_level_statement] = STATE(53),
    [sym_let_binding] = STATE(52),
    [sym_operation_call] = STATE(52),
    [sym_use_clause] = STATE(7),
    [sym_qualified_identifier] = STATE(51),
    [sym_operation_name] = STATE(37),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_use] = ACTIONS(7),
    [sym_identifier] = ACTIONS(9),
    [sym_comment] = ACTIONS(3),
    [sym_whitespace] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    ACTIONS(15), 1,
      anon_sym_RBRACK,
    STATE(18), 1,
      sym_input_node,
    STATE(47), 1,
      sym_list_fields,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(19), 3,
      sym_number,
      sym_string,
      sym_null,
    STATE(12), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [34] = 8,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    ACTIONS(21), 1,
      anon_sym_RBRACK,
    STATE(38), 1,
      sym_input_node,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(19), 3,
      sym_number,
      sym_string,
      sym_null,
    STATE(12), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [65] = 8,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    ACTIONS(23), 1,
      anon_sym_RBRACK,
    STATE(38), 1,
      sym_input_node,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(19), 3,
      sym_number,
      sym_string,
      sym_null,
    STATE(12), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [96] = 7,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    STATE(38), 1,
      sym_input_node,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(19), 3,
      sym_number,
      sym_string,
      sym_null,
    STATE(12), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [124] = 7,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    STATE(29), 1,
      sym_input_node,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(19), 3,
      sym_number,
      sym_string,
      sym_null,
    STATE(12), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [152] = 7,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(9), 1,
      sym_identifier,
    STATE(37), 1,
      sym_operation_name,
    STATE(48), 1,
      sym_top_level_statement,
    STATE(51), 1,
      sym_qualified_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    STATE(52), 2,
      sym_let_binding,
      sym_operation_call,
  [176] = 5,
    ACTIONS(25), 1,
      anon_sym_RBRACE,
    ACTIONS(27), 1,
      sym_identifier,
    STATE(17), 1,
      sym_binding,
    STATE(41), 1,
      sym_bindings,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [193] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(29), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [204] = 5,
    ACTIONS(31), 1,
      anon_sym_DOT,
    ACTIONS(33), 1,
      anon_sym_POUND,
    ACTIONS(35), 1,
      anon_sym_LBRACE,
    STATE(22), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [221] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(37), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [232] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(39), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [243] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(41), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [254] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(43), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [265] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(45), 4,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
  [276] = 4,
    ACTIONS(23), 1,
      anon_sym_RBRACK,
    ACTIONS(47), 1,
      anon_sym_COMMA,
    STATE(28), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [290] = 4,
    ACTIONS(49), 1,
      anon_sym_COMMA,
    ACTIONS(51), 1,
      anon_sym_RBRACE,
    STATE(21), 1,
      aux_sym_bindings_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [304] = 4,
    ACTIONS(53), 1,
      anon_sym_COMMA,
    ACTIONS(55), 1,
      anon_sym_RBRACK,
    STATE(16), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [318] = 4,
    ACTIONS(57), 1,
      anon_sym_COMMA,
    ACTIONS(60), 1,
      anon_sym_RBRACE,
    STATE(19), 1,
      aux_sym_bindings_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [332] = 4,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(62), 1,
      anon_sym_RBRACE,
    STATE(30), 1,
      sym_binding,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [346] = 4,
    ACTIONS(64), 1,
      anon_sym_COMMA,
    ACTIONS(66), 1,
      anon_sym_RBRACE,
    STATE(19), 1,
      aux_sym_bindings_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [360] = 4,
    ACTIONS(31), 1,
      anon_sym_DOT,
    ACTIONS(68), 1,
      anon_sym_POUND,
    STATE(26), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [374] = 4,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(66), 1,
      anon_sym_RBRACE,
    STATE(30), 1,
      sym_binding,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [388] = 3,
    ACTIONS(72), 1,
      anon_sym_DOT,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(70), 2,
      anon_sym_let,
      sym_identifier,
  [400] = 3,
    ACTIONS(76), 1,
      anon_sym_DOT,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(74), 2,
      anon_sym_let,
      sym_identifier,
  [412] = 4,
    ACTIONS(78), 1,
      anon_sym_DOT,
    ACTIONS(81), 1,
      anon_sym_POUND,
    STATE(26), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [426] = 4,
    ACTIONS(31), 1,
      anon_sym_DOT,
    ACTIONS(33), 1,
      anon_sym_POUND,
    STATE(22), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [440] = 4,
    ACTIONS(83), 1,
      anon_sym_COMMA,
    ACTIONS(86), 1,
      anon_sym_RBRACK,
    STATE(28), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [454] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(88), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RBRACE,
  [464] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(60), 2,
      anon_sym_COMMA,
      anon_sym_RBRACE,
  [473] = 3,
    ACTIONS(27), 1,
      sym_identifier,
    STATE(30), 1,
      sym_binding,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [484] = 3,
    ACTIONS(90), 1,
      sym_identifier,
    STATE(35), 1,
      sym_qualified_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [495] = 3,
    ACTIONS(92), 1,
      ts_builtin_sym_end,
    ACTIONS(94), 1,
      anon_sym_COMMA,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [506] = 3,
    ACTIONS(27), 1,
      sym_identifier,
    STATE(33), 1,
      sym_binding,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [517] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(96), 2,
      anon_sym_let,
      sym_identifier,
  [526] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(81), 2,
      anon_sym_DOT,
      anon_sym_POUND,
  [535] = 3,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    STATE(45), 1,
      sym_struct,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [546] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(86), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
  [555] = 2,
    ACTIONS(98), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [563] = 2,
    ACTIONS(100), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [571] = 2,
    ACTIONS(102), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [579] = 2,
    ACTIONS(104), 1,
      anon_sym_LBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [587] = 2,
    ACTIONS(106), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [595] = 2,
    ACTIONS(108), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [603] = 2,
    ACTIONS(110), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [611] = 2,
    ACTIONS(112), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [619] = 2,
    ACTIONS(114), 1,
      anon_sym_RBRACK,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [627] = 2,
    ACTIONS(116), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [635] = 2,
    ACTIONS(118), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [643] = 2,
    ACTIONS(120), 1,
      anon_sym_service,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [651] = 2,
    ACTIONS(122), 1,
      anon_sym_DOT,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [659] = 2,
    ACTIONS(124), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [667] = 2,
    ACTIONS(126), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [675] = 2,
    ACTIONS(128), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [683] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(130), 1,
      sym_whitespace,
  [690] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(132), 1,
      sym_whitespace,
  [697] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(134), 1,
      sym_whitespace,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 34,
  [SMALL_STATE(4)] = 65,
  [SMALL_STATE(5)] = 96,
  [SMALL_STATE(6)] = 124,
  [SMALL_STATE(7)] = 152,
  [SMALL_STATE(8)] = 176,
  [SMALL_STATE(9)] = 193,
  [SMALL_STATE(10)] = 204,
  [SMALL_STATE(11)] = 221,
  [SMALL_STATE(12)] = 232,
  [SMALL_STATE(13)] = 243,
  [SMALL_STATE(14)] = 254,
  [SMALL_STATE(15)] = 265,
  [SMALL_STATE(16)] = 276,
  [SMALL_STATE(17)] = 290,
  [SMALL_STATE(18)] = 304,
  [SMALL_STATE(19)] = 318,
  [SMALL_STATE(20)] = 332,
  [SMALL_STATE(21)] = 346,
  [SMALL_STATE(22)] = 360,
  [SMALL_STATE(23)] = 374,
  [SMALL_STATE(24)] = 388,
  [SMALL_STATE(25)] = 400,
  [SMALL_STATE(26)] = 412,
  [SMALL_STATE(27)] = 426,
  [SMALL_STATE(28)] = 440,
  [SMALL_STATE(29)] = 454,
  [SMALL_STATE(30)] = 464,
  [SMALL_STATE(31)] = 473,
  [SMALL_STATE(32)] = 484,
  [SMALL_STATE(33)] = 495,
  [SMALL_STATE(34)] = 506,
  [SMALL_STATE(35)] = 517,
  [SMALL_STATE(36)] = 526,
  [SMALL_STATE(37)] = 535,
  [SMALL_STATE(38)] = 546,
  [SMALL_STATE(39)] = 555,
  [SMALL_STATE(40)] = 563,
  [SMALL_STATE(41)] = 571,
  [SMALL_STATE(42)] = 579,
  [SMALL_STATE(43)] = 587,
  [SMALL_STATE(44)] = 595,
  [SMALL_STATE(45)] = 603,
  [SMALL_STATE(46)] = 611,
  [SMALL_STATE(47)] = 619,
  [SMALL_STATE(48)] = 627,
  [SMALL_STATE(49)] = 635,
  [SMALL_STATE(50)] = 643,
  [SMALL_STATE(51)] = 651,
  [SMALL_STATE(52)] = 659,
  [SMALL_STATE(53)] = 667,
  [SMALL_STATE(54)] = 675,
  [SMALL_STATE(55)] = 683,
  [SMALL_STATE(56)] = 690,
  [SMALL_STATE(57)] = 697,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(55),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(57),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 3),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 2),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_name, 1, .production_id = 1),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3, .production_id = 11),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_input_node, 1),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 3, .production_id = 8),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 2),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [49] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bindings, 1),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 1),
  [57] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_bindings_repeat1, 2), SHIFT_REPEAT(31),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_bindings_repeat1, 2),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bindings, 3),
  [64] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bindings, 2),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [70] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_identifier, 3, .production_id = 5),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 3, .production_id = 5),
  [74] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_identifier, 4, .production_id = 7),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 4, .production_id = 7),
  [78] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2), SHIFT_REPEAT(39),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2),
  [83] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_list_fields_repeat1, 2), SHIFT_REPEAT(5),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_list_fields_repeat1, 2),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binding, 3, .production_id = 9),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [92] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_binding, 3),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [96] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_use_clause, 5, .production_id = 10),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_binding, 4),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_name, 3, .production_id = 6),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_call, 2, .production_id = 4),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2, .production_id = 3),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [124] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_top_level_statement, 1),
  [126] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, .production_id = 2),
  [128] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_smithyql(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
