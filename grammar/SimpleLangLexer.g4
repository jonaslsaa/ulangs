lexer grammar SimpleLangLexer;

// Keywords
DEF: 'def';
RET: 'ret';
FOR: 'for';
IN: 'in';
IF: 'if';
ELSE: 'else';
PRINT: 'print';

// Types
INT_TYPE: 'int';
STRING_TYPE: 'string';
LIST_TYPE: 'List';

// Operators
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
ASSIGN: '=';
LE: '<=';

// Delimiters
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
COLON: ':';
COMMA: ',';
LT: '<';
GT: '>';

// Literals
INTEGER: [0-9]+;
STRING: '"' (~["\r\n])* '"';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Comments
COMMENT: '#' ~[\r\n]* -> skip;

// Whitespace
WS: [ \t\r\n]+ -> skip;
