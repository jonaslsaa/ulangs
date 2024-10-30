lexer grammar MyLexer;

// Keywords
PRINT: 'print';
DEF: 'def';
RET: 'ret';
IF: 'if';
ELSE: 'else';
FOR: 'for';
IN: 'in';

// Type keywords
INT: 'int';
STRING_TYPE: 'string';
LIST: 'List';

// Operators
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
LE: '<=';
GE: '>=';
EQ: '==';
NE: '!=';
LT: '<';
GT: '>';
ASSIGN: '=';

// Delimiters
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
COLON: ':';
COMMA: ',';

// Literals
NUMBER: [0-9]+;
STRING: '"' (~["\r\n\\] | '\\' .)* '"';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Comments
COMMENT: ('#' | '//') ~[\r\n]* -> skip;

// Whitespace
WS: [ \t\r\n]+ -> skip;
