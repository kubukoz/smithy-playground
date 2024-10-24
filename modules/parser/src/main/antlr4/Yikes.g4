parser grammar Yikes;
options {
	tokenVocab = Tokens;
}

namespace: (ID ('.' ID)*);
qualified_identifier: namespace '#' ID;
use_clause: 'use' 'service' qualified_identifier;

source_file: use_clause* EOF;
