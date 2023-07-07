parser grammar SmithyQL;
options {
	tokenVocab = Tokens;
}

qualified_identifier: ident ('.' ident)* '#' ident;

soft_keyword: 'use' | 'service';

ident: ID | soft_keyword;

use_clause: 'use' 'service' qualified_identifier;

prelude: use_clause*;

service_reference: (qualified_identifier '.')? ident;

query_operation_name: service_reference;

number: NUMBER;
bool: 'true' | 'false';
node: number | bool | STRING | NULL | struct | listed;

field: key = ident (':' | '=') value = node;

struct: '{' (field (',' field)* (',')?)? '}';

listed: '[' (node (',' node)* (',')?)? ']';

query: query_operation_name struct;

run_query: query;

statement: run_query;

source_file: prelude statement* EOF;
