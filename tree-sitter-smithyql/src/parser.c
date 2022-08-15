#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 43
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 32
#define ALIAS_COUNT 0
#define TOKEN_COUNT 19
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 3

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
  sym_list_field = 29,
  sym_boolean = 30,
  aux_sym_qualified_identifier_repeat1 = 31,
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
  [sym_list_field] = "list_field",
  [sym_boolean] = "boolean",
  [aux_sym_qualified_identifier_repeat1] = "qualified_identifier_repeat1",
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
  [sym_list_field] = sym_list_field,
  [sym_boolean] = sym_boolean,
  [aux_sym_qualified_identifier_repeat1] = aux_sym_qualified_identifier_repeat1,
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
  [sym_list_field] = {
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
};

enum {
  field_input = 1,
  field_operation_name = 2,
  field_use_clause = 3,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_input] = "input",
  [field_operation_name] = "operation_name",
  [field_use_clause] = "use_clause",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_input, 1},
    {field_operation_name, 0},
  [2] =
    {field_input, 2},
    {field_operation_name, 1},
    {field_use_clause, 0},
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
  [5] = {.lex_state = 3},
  [6] = {.lex_state = 3},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
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
  [20] = {.lex_state = 3},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 3},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 3},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 3},
  [28] = {.lex_state = 3},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 3},
  [35] = {.lex_state = 3},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 1},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 3},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
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
    [sym_source_file] = STATE(29),
    [sym_use_clause] = STATE(20),
    [sym_operation_name] = STATE(17),
    [ts_builtin_sym_end] = ACTIONS(5),
    [anon_sym_use] = ACTIONS(7),
    [sym_identifier] = ACTIONS(9),
    [sym_comment] = ACTIONS(3),
    [sym_whitespace] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 10,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    ACTIONS(15), 1,
      anon_sym_RBRACK,
    STATE(16), 1,
      sym_list_field,
    STATE(23), 1,
      sym_input_node,
    STATE(40), 1,
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
    STATE(14), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [37] = 10,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    ACTIONS(21), 1,
      anon_sym_RBRACK,
    STATE(16), 1,
      sym_list_field,
    STATE(23), 1,
      sym_input_node,
    STATE(32), 1,
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
    STATE(14), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [74] = 7,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    ACTIONS(13), 1,
      anon_sym_LBRACK,
    STATE(24), 1,
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
    STATE(14), 3,
      sym_struct,
      sym_list,
      sym_boolean,
  [102] = 5,
    ACTIONS(23), 1,
      anon_sym_RBRACE,
    ACTIONS(25), 1,
      sym_identifier,
    STATE(21), 1,
      sym_field,
    STATE(33), 1,
      sym_fields,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [119] = 5,
    ACTIONS(25), 1,
      sym_identifier,
    ACTIONS(27), 1,
      anon_sym_RBRACE,
    STATE(21), 1,
      sym_field,
    STATE(26), 1,
      sym_fields,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [136] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(29), 4,
      ts_builtin_sym_end,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [147] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(31), 4,
      ts_builtin_sym_end,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [158] = 4,
    ACTIONS(33), 1,
      anon_sym_DOT,
    ACTIONS(35), 1,
      anon_sym_POUND,
    STATE(12), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [172] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(37), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [182] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(39), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [192] = 4,
    ACTIONS(41), 1,
      anon_sym_DOT,
    ACTIONS(44), 1,
      anon_sym_POUND,
    STATE(12), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [206] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(46), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [216] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(48), 3,
      anon_sym_RBRACE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [226] = 4,
    ACTIONS(33), 1,
      anon_sym_DOT,
    ACTIONS(50), 1,
      anon_sym_POUND,
    STATE(9), 1,
      aux_sym_qualified_identifier_repeat1,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [240] = 3,
    ACTIONS(52), 1,
      anon_sym_RBRACK,
    ACTIONS(54), 1,
      anon_sym_COMMA,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [251] = 3,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    STATE(30), 1,
      sym_struct,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [262] = 3,
    ACTIONS(11), 1,
      anon_sym_LBRACE,
    STATE(31), 1,
      sym_struct,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [273] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(44), 2,
      anon_sym_DOT,
      anon_sym_POUND,
  [282] = 3,
    ACTIONS(56), 1,
      sym_identifier,
    STATE(18), 1,
      sym_operation_name,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [293] = 3,
    ACTIONS(58), 1,
      anon_sym_RBRACE,
    ACTIONS(60), 1,
      anon_sym_COMMA,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [304] = 3,
    ACTIONS(62), 1,
      sym_identifier,
    STATE(25), 1,
      sym_qualified_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [315] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(64), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [324] = 2,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
    ACTIONS(66), 2,
      anon_sym_RBRACE,
      anon_sym_COMMA,
  [333] = 2,
    ACTIONS(68), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [341] = 2,
    ACTIONS(70), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [349] = 2,
    ACTIONS(72), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [357] = 2,
    ACTIONS(74), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [365] = 2,
    ACTIONS(76), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [373] = 2,
    ACTIONS(78), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [381] = 2,
    ACTIONS(80), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [389] = 2,
    ACTIONS(82), 1,
      anon_sym_RBRACK,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [397] = 2,
    ACTIONS(84), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [405] = 2,
    ACTIONS(86), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [413] = 2,
    ACTIONS(88), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [421] = 2,
    ACTIONS(90), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [429] = 2,
    ACTIONS(92), 1,
      anon_sym_service,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [437] = 2,
    ACTIONS(94), 1,
      anon_sym_LBRACE,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [445] = 2,
    ACTIONS(96), 1,
      sym_identifier,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [453] = 2,
    ACTIONS(98), 1,
      anon_sym_RBRACK,
    ACTIONS(3), 2,
      sym_comment,
      sym_whitespace,
  [461] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(100), 1,
      sym_whitespace,
  [468] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(102), 1,
      sym_whitespace,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 37,
  [SMALL_STATE(4)] = 74,
  [SMALL_STATE(5)] = 102,
  [SMALL_STATE(6)] = 119,
  [SMALL_STATE(7)] = 136,
  [SMALL_STATE(8)] = 147,
  [SMALL_STATE(9)] = 158,
  [SMALL_STATE(10)] = 172,
  [SMALL_STATE(11)] = 182,
  [SMALL_STATE(12)] = 192,
  [SMALL_STATE(13)] = 206,
  [SMALL_STATE(14)] = 216,
  [SMALL_STATE(15)] = 226,
  [SMALL_STATE(16)] = 240,
  [SMALL_STATE(17)] = 251,
  [SMALL_STATE(18)] = 262,
  [SMALL_STATE(19)] = 273,
  [SMALL_STATE(20)] = 282,
  [SMALL_STATE(21)] = 293,
  [SMALL_STATE(22)] = 304,
  [SMALL_STATE(23)] = 315,
  [SMALL_STATE(24)] = 324,
  [SMALL_STATE(25)] = 333,
  [SMALL_STATE(26)] = 341,
  [SMALL_STATE(27)] = 349,
  [SMALL_STATE(28)] = 357,
  [SMALL_STATE(29)] = 365,
  [SMALL_STATE(30)] = 373,
  [SMALL_STATE(31)] = 381,
  [SMALL_STATE(32)] = 389,
  [SMALL_STATE(33)] = 397,
  [SMALL_STATE(34)] = 405,
  [SMALL_STATE(35)] = 413,
  [SMALL_STATE(36)] = 421,
  [SMALL_STATE(37)] = 429,
  [SMALL_STATE(38)] = 437,
  [SMALL_STATE(39)] = 445,
  [SMALL_STATE(40)] = 453,
  [SMALL_STATE(41)] = 461,
  [SMALL_STATE(42)] = 468,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 2),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 2),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 3),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_struct, 2),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3),
  [41] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2), SHIFT_REPEAT(27),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_identifier_repeat1, 2),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_input_node, 1),
  [50] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [52] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 1),
  [54] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [56] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 1),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [62] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [64] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_field, 1),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field, 3),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_clause, 5),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fields, 3),
  [72] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [76] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2, .production_id = 1),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 3, .production_id = 2),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 3),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [94] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operation_name, 1),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_identifier, 4),
  [98] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_fields, 3),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
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
