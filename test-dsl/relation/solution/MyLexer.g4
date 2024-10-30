lexer grammar MyLexer;

// Comments
COMMENT: '#' ~[\r\n]* -> skip;

// Keywords and functions
IF: 'if';
OUTPUT: '$output';

// Symbols
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
COMMA: ',';
DOT: '.';
EQUALS: '=';
ARROW: '-->';
REL_START: '--[';
REL_END: ']-->';
GT: '>';

// Variables
VARIABLE: '$' [a-zA-Z][a-zA-Z0-9_]*;

// Identifiers and literals
IDENTIFIER: [a-zA-Z][a-zA-Z0-9_]*;
STRING: '"' (~["\r\n])* '"';
NUMBER: [0-9]+;

// Whitespace
WS: [ \t\r\n]+ -> skip;
