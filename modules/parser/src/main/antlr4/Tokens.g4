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
NUMBER: '-'? [0-9]+ (. [0-9]+)?;
STRING_LITERAL:
	'"' (
		~["\r\n]
		| '\\"'
		| '\\\\'
		| '\\/'
		| '\\b'
		| '\\f'
		| '\\n'
		| '\\r'
		| '\\t'
		| '\\u' HEX HEX HEX HEX
	)* '"';
HEX: [0-9a-fA-F];

WS: [ \t\n\r\f]+ -> skip;
