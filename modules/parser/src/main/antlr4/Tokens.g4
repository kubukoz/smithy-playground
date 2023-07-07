lexer grammar Tokens;

DOT: '.';
COMMA: ',';
USE: 'use';
SERVICE: 'service';
HASH: '#';
LBRACE: '{';
RBRACE: '}';
LBRACKET: '[';
RBRACKET: ']';
EQUAL: '=';
COLON: ':';
TRUE: 'true';
FALSE: 'false';
NULL: 'null';

ID: [a-zA-Z][a-zA-Z_0-9]*;

// string and number tokens stolen from JSON
// https://github.com/antlr/grammars-v4/blob/c9e5c8caca0b562805711b22d04a8ce39d012461/json/JSON.g4
STRING: '"' (ESC | SAFECODEPOINT)* '"';

fragment ESC: '\\' (["\\/bfnrt] | UNICODE);

fragment UNICODE: 'u' HEX HEX HEX HEX;

fragment HEX: [0-9a-fA-F];

fragment SAFECODEPOINT: ~ ["\\\u0000-\u001F];

NUMBER: '-'? INT ('.' [0-9]+)? EXP?;

fragment INT: // integer part forbids leading 0s (e.g. `01`)
	'0'
	| [1-9] [0-9]*;

// no leading zeros
fragment EXP: // exponent number permits leading 0s (e.g. `1e01`)
	[Ee] [+\-]? [0-9]+;

WS: [ \t\n\r\f]+ -> skip;
