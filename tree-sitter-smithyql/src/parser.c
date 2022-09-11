#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 53
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 33
#define ALIAS_COUNT 0
#define TOKEN_COUNT 19
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 12
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 11

enum {
  anon_sym_use = 1,
  anon_sym_service = 2,
  anon_sym_DOT = 3,
  anon_sym_POUND = 4,
  anon_sym_LBRACE = 5,
  anon_sym_RBRACE = 6,
  anon_sym_LBRACK = 7,
  anon_sym_RBRACK = 8,
  anon_sym_COMMA = 9,
  anon_sym_EQ = 10,
  sym_identifier = 11,
  anon_sym_true = 12,
  anon_sym_false = 13,
  sym_number = 14,
  sym_string = 15,
  sym_null = 16,
  sym_comment = 17,
  sym_whitespace = 18,
  sym_source_file = 19,
  sym_use_clause = 20,
  sym_qualified_identifier = 21,
  sym_operation_name = 22,
  sym_input_node = 23,
  sym_struct = 24,
  sym_list = 25,
  sym_fields = 26,
  sym_field = 27,
  sym_list_fields = 28,
  sym_boolean = 29,
  aux_sym_qualified_identifier_repeat1 = 30,
  aux_sym_fields_repeat1 = 31,
  aux_sym_list_fields_repeat1 = 32,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_use] = "use",
  [anon_sym_service] = "service",
  [anon_sym_DOT] = ".",
  [anon_sym_POUND] = "#",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_COMMA] = ",",
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
  [sym_use_clause] = "use_clause",
  [sym_qualified_identifier] = "qualified_identifier",
  [sym_operation_name] = "operation_name",
  [sym_input_node] = "input_node",
  [sym_struct] = "struct",
  [sym_list] = "list",
  [sym_fields] = "fields",
  [sym_field] = "field",
  [sym_list_fields] = "list_fields",
  [sym_boolean] = "boolean",
  [aux_sym_qualified_identifier_repeat1] = "qualified_identifier_repeat1",
  [aux_sym_fields_repeat1] = "fields_repeat1",
  [aux_sym_list_fields_repeat1] = "list_fields_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_use] = anon_sym_use,
  [anon_sym_service] = anon_sym_service,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_POUND] = anon_sym_POUND,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_COMMA] = anon_sym_COMMA,
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
  [sym_use_clause] = sym_use_clause,
  [sym_qualified_identifier] = sym_qualified_identifier,
  [sym_operation_name] = sym_operation_name,
  [sym_input_node] = sym_input_node,
  [sym_struct] = sym_struct,
  [sym_list] = sym_list,
  [sym_fields] = sym_fields,
  [sym_field] = sym_field,
  [sym_list_fields] = sym_list_fields,
  [sym_boolean] = sym_boolean,
  [aux_sym_qualified_identifier_repeat1] = aux_sym_qualified_identifier_repeat1,
  [aux_sym_fields_repeat1] = aux_sym_fields_repeat1,
  [aux_sym_list_fields_repeat1] = aux_sym_list_fields_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
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
  [anon_sym_COMMA] = {
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
  [sym_fields] = {
    .visible = true,
    .named = true,
  },
  [sym_field] = {
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
  [aux_sym_fields_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_list_fields_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_fields = 1,
  field_head = 2,
  field_identifier = 3,
  field_input = 4,
  field_key = 5,
  field_list_fields = 6,
  field_name = 7,
  field_operation_name = 8,
  field_selection = 9,
  field_tail = 10,
  field_use_clause = 11,
  field_value = 12,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_fields] = "fields",
  [field_head] = "head",
  [field_identifier] = "identifier",
  [field_input] = "input",
  [field_key] = "key",
  [field_list_fields] = "list_fields",
  [field_name] = "name",
  [field_operation_name] = "operation_name",
  [field_selection] = "selection",
  [field_tail] = "tail",
  [field_use_clause] = "use_clause",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 2},
  [4] = {.index = 5, .length = 3},
  [5] = {.index = 8, .length = 3},
  [6] = {.index = 11, .length = 3},
  [7] = {.index = 14, .length = 1},
  [8] = {.index = 15, .length = 1},
  [9] = {.index = 16, .length = 2},
  [10] = {.index = 18, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_input, 1},
    {field_operation_name, 0},
  [3] =
    {field_head, 0},
    {field_selection, 2},
  [5] =
    {field_input, 2},
    {field_operation_name, 1},
    {field_use_clause, 0},
  [8] =
    {field_identifier, 0},
    {field_identifier, 1},
    {field_name, 2},
  [11] =
    {field_head, 0},
    {field_selection, 3},
    {field_tail, 1},
  [14] =
    {field_fields, 1},
  [15] =
    {field_identifier, 4},
  [16] =
    {field_key, 0},
    {field_value, 2},
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
      if (eof) ADVANCE(27);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '#') ADVANCE(32);
      if (lookahead == ',') ADVANCE(37);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '.') ADVANCE(31);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == '0') ADVANCE(62);
      if (lookahead == '=') ADVANCE(38);
      if (lookahead == '[') ADVANCE(35);
      if (lookahead == ']') ADVANCE(36);
      if (lookahead == 'f') ADVANCE(39);
      if (lookahead == 'n') ADVANCE(54);
      if (lookahead == 's') ADVANCE(45);
      if (lookahead == 't') ADVANCE(51);
      if (lookahead == 'u') ADVANCE(52);
      if (lookahead == '{') ADVANCE(33);
      if (lookahead == '}') ADVANCE(34);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(70);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(63);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == '0') ADVANCE(62);
      if (lookahead == '[') ADVANCE(35);
      if (lookahead == ']') ADVANCE(36);
      if (lookahead == 'f') ADVANCE(6);
      if (lookahead == 'n') ADVANCE(20);
      if (lookahead == 's') ADVANCE(11);
      if (lookahead == 't') ADVANCE(17);
      if (lookahead == '{') ADVANCE(33);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(70);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(63);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(66);
      if (lookahead == '\\') ADVANCE(25);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (lookahead == '.') ADVANCE(31);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == '}') ADVANCE(34);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(70);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 4:
      if (lookahead == '/') ADVANCE(69);
      END_STATE();
    case 5:
      if (lookahead == '0') ADVANCE(62);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(63);
      END_STATE();
    case 6:
      if (lookahead == 'a') ADVANCE(13);
      END_STATE();
    case 7:
      if (lookahead == 'c') ADVANCE(10);
      END_STATE();
    case 8:
      if (lookahead == 'e') ADVANCE(58);
      END_STATE();
    case 9:
      if (lookahead == 'e') ADVANCE(60);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(29);
      END_STATE();
    case 11:
      if (lookahead == 'e') ADVANCE(16);
      END_STATE();
    case 12:
      if (lookahead == 'i') ADVANCE(7);
      END_STATE();
    case 13:
      if (lookahead == 'l') ADVANCE(18);
      END_STATE();
    case 14:
      if (lookahead == 'l') ADVANCE(67);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(14);
      END_STATE();
    case 16:
      if (lookahead == 'r') ADVANCE(21);
      END_STATE();
    case 17:
      if (lookahead == 'r') ADVANCE(19);
      END_STATE();
    case 18:
      if (lookahead == 's') ADVANCE(9);
      END_STATE();
    case 19:
      if (lookahead == 'u') ADVANCE(8);
      END_STATE();
    case 20:
      if (lookahead == 'u') ADVANCE(15);
      END_STATE();
    case 21:
      if (lookahead == 'v') ADVANCE(12);
      END_STATE();
    case 22:
      if (lookahead == '+' ||
          lookahead == '-') ADVANCE(24);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(65);
      END_STATE();
    case 23:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(64);
      END_STATE();
    case 24:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(65);
      END_STATE();
    case 25:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(2);
      END_STATE();
    case 26:
      if (eof) ADVANCE(27);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == 'u') ADVANCE(52);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(70);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_use);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_service);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_service);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_POUND);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(49);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(59);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(61);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(50);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(40);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(68);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(47);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(41);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(43);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(48);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(42);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'v') ADVANCE(46);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(23);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(22);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(23);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(22);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(63);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(22);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(64);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(65);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(sym_null);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(sym_null);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(57);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(69);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_whitespace);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(70);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 26},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 1},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 3},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 3},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 3},
  [25] = {.lex_state = 3},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 3},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 3},
  [32] = {.lex_state = 3},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 3},
  [37] = {.lex_state = 3},
  [38] = {.lex_state = 3},
  [39] = {.lex_state = 3},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 3},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 3},
  [46] = {.lex_state = 1},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 0},
  [52] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_use] = ACTIONS(1),
    [anon_sym_service] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_POUND] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
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
    [sym_source_file] = STATE(50),
    [sym_use_clause] = STATE(25),
    [sym_qualified_identifier] = STATE(49),
    [sym_operation_name] = STATE(29),
    [ts_builtin_sym_end] = ACTIONS(5),
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
    STATE(13), 1,
      sym_input_node,
    STATE(42), 1,
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
    STATE(18), 3,
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
    STATE(30), 1,
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
    STATE(18), 3,
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
    STATE(30), 1,
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
    STATE(18), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [96] = 7,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    STATE(34), 1,
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
    STATE(18), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [124] = 7,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    STATE(30), 1,
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
    STATE(18), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [152] = 5,
    ACTIONS(25), 1,
      anon_sym_RBRACE,
    ACTIONS(27), 1,
      sym_identifier,
    STATE(26), 1,
      sym_field,
    STATE(48), 1,
      sym_fields,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [169] = 5,
    ACTIONS(29), 1,
      anon_sym_DOT,
    ACTIONS(31), 1,
      anon_sym_POUND,
    ACTIONS(33), 1,
      anon_sym_LBRACE,
    STATE(14), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [186] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(35), 4,
      ts_builtin_sym_end,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [197] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(37), 4,
      ts_builtin_sym_end,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [208] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(39), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [218] = 4,
    ACTIONS(23), 1,
      anon_sym_RBRACK,
    ACTIONS(41), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [232] = 4,
    ACTIONS(43), 1,
      anon_sym_RBRACK,
    ACTIONS(45), 1,
      anon_sym_COMMA,
    STATE(12), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [246] = 4,
    ACTIONS(29), 1,
      anon_sym_DOT,
    ACTIONS(47), 1,
      anon_sym_POUND,
    STATE(22), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [260] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(49), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [270] = 4,
    ACTIONS(51), 1,
      anon_sym_RBRACE,
    ACTIONS(53), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [284] = 4,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(56), 1,
      anon_sym_RBRACE,
    STATE(35), 1,
      sym_field,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [298] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(58), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [308] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(60), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [318] = 4,
    ACTIONS(62), 1,
      anon_sym_RBRACK,
    ACTIONS(64), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      aux_sym_list_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [332] = 4,
    ACTIONS(29), 1,
      anon_sym_DOT,
    ACTIONS(31), 1,
      anon_sym_POUND,
    STATE(14), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [346] = 4,
    ACTIONS(67), 1,
      anon_sym_DOT,
    ACTIONS(70), 1,
      anon_sym_POUND,
    STATE(22), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [360] = 4,
    ACTIONS(72), 1,
      anon_sym_RBRACE,
    ACTIONS(74), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [374] = 4,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(72), 1,
      anon_sym_RBRACE,
    STATE(35), 1,
      sym_field,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [388] = 4,
    ACTIONS(76), 1,
      sym_identifier,
    STATE(33), 1,
      sym_operation_name,
    STATE(49), 1,
      sym_qualified_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [402] = 4,
    ACTIONS(78), 1,
      anon_sym_RBRACE,
    ACTIONS(80), 1,
      anon_sym_COMMA,
    STATE(23), 1,
      aux_sym_fields_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [416] = 3,
    ACTIONS(82), 1,
      sym_identifier,
    STATE(38), 1,
      sym_qualified_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [427] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(70), 2,
      anon_sym_DOT,
      anon_sym_POUND,
  [436] = 3,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    STATE(40), 1,
      sym_struct,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [447] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(62), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [456] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(84), 2,
      anon_sym_DOT,
      sym_identifier,
  [465] = 3,
    ACTIONS(27), 1,
      sym_identifier,
    STATE(35), 1,
      sym_field,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [476] = 3,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    STATE(43), 1,
      sym_struct,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [487] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(86), 2,
      anon_sym_RBRACE,
      anon_sym_COMMA,
  [496] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(51), 2,
      anon_sym_RBRACE,
      anon_sym_COMMA,
  [505] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(88), 2,
      anon_sym_DOT,
      sym_identifier,
  [514] = 2,
    ACTIONS(90), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [522] = 2,
    ACTIONS(92), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [530] = 2,
    ACTIONS(94), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [538] = 2,
    ACTIONS(96), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [546] = 2,
    ACTIONS(98), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [554] = 2,
    ACTIONS(100), 1,
      anon_sym_RBRACK,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [562] = 2,
    ACTIONS(102), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [570] = 2,
    ACTIONS(104), 1,
      anon_sym_LBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [578] = 2,
    ACTIONS(106), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [586] = 2,
    ACTIONS(108), 1,
      anon_sym_service,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [594] = 2,
    ACTIONS(110), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [602] = 2,
    ACTIONS(112), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [610] = 2,
    ACTIONS(114), 1,
      anon_sym_DOT,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [618] = 2,
    ACTIONS(116), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [626] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(118), 1,
      sym_whitespace,
  [633] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(120), 1,
      sym_whitespace,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 34,
  [SMALL_STATE(4)] = 65,
  [SMALL_STATE(5)] = 96,
  [SMALL_STATE(6)] = 124,
  [SMALL_STATE(7)] = 152,
  [SMALL_STATE(8)] = 169,
  [SMALL_STATE(9)] = 186,
  [SMALL_STATE(10)] = 197,
  [SMALL_STATE(11)] = 208,
  [SMALL_STATE(12)] = 218,
  [SMALL_STATE(13)] = 232,
  [SMALL_STATE(14)] = 246,
  [SMALL_STATE(15)] = 260,
  [SMALL_STATE(16)] = 270,
  [SMALL_STATE(17)] = 284,
  [SMALL_STATE(18)] = 298,
  [SMALL_STATE(19)] = 308,
  [SMALL_STATE(20)] = 318,
  [SMALL_STATE(21)] = 332,
  [SMALL_STATE(22)] = 346,
  [SMALL_STATE(23)] = 360,
  [SMALL_STATE(24)] = 374,
  [SMALL_STATE(25)] = 388,
  [SMALL_STATE(26)] = 402,
  [SMALL_STATE(27)] = 416,
  [SMALL_STATE(28)] = 427,
  [SMALL_STATE(29)] = 436,
  [SMALL_STATE(30)] = 447,
  [SMALL_STATE(31)] = 456,
  [SMALL_STATE(32)] = 465,
  [SMALL_STATE(33)] = 476,
  [SMALL_STATE(34)] = 487,
  [SMALL_STATE(35)] = 496,
  [SMALL_STATE(36)] = 505,
  [SMALL_STATE(37)] = 514,
  [SMALL_STATE(38)] = 522,
  [SMALL_STATE(39)] = 530,
  [SMALL_STATE(40)] = 538,
  [SMALL_STATE(41)] = 546,
  [SMALL_STATE(42)] = 554,
  [SMALL_STATE(43)] = 562,
  [SMALL_STATE(44)] = 570,
  [SMALL_STATE(45)] = 578,
  [SMALL_STATE(46)] = 586,
  [SMALL_STATE(47)] = 594,
  [SMALL_STATE(48)] = 602,
  [SMALL_STATE(49)] = 610,
  [SMALL_STATE(50)] = 618,
  [SMALL_STATE(51)] = 626,
  [SMALL_STATE(52)] = 633,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(52),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 3),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 2),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_name, 1, .production_id = 1),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 2),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 3, .production_id = 7),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3, .production_id = 10),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 1),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fields_repeat1, 2),
  [53] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fields_repeat1, 2), SHIFT_REPEAT(32),
  [56] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 3),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_input_node, 1),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_list_fields_repeat1, 2),
  [64] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_list_fields_repeat1, 2), SHIFT_REPEAT(6),
  [67] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2), SHIFT_REPEAT(45),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 2),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 1),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 4, .production_id = 6),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field, 3, .production_id = 9),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 3, .production_id = 3),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [92] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_clause, 5, .production_id = 8),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2, .production_id = 2),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [102] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 3, .production_id = 4),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_name, 3, .production_id = 5),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [116] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
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
