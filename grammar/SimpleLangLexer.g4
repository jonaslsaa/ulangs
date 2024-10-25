lexer grammar SimpleLangLexer;

// Keywords
DEF: 'def';
RET: 'ret';

// Operators
PLUS: '+';
ASSIGN: '=';
DOT: '.';

// Punctuation
LPAREN: '(';
RPAREN: ')';
COLON: ':';
COMMA: ',';
LBRACKET: '[';
RBRACKET: ']';

// Identifiers and Literals
ID: [a-zA-Z_][a-zA-Z_0-9]*;
NUMBER: [0-9]+;

// Whitespace
WS: [ \t\r\n]+ -> skip;