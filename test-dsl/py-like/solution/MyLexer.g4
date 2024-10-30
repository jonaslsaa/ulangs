lexer grammar MyLexer;

// Keywords
DEF: 'def';
RET: 'ret';
IF: 'if';
ELSE: 'else';
FOR: 'for';
IN: 'in';
PRINT: 'print';
INT: 'int';
STRING_TYPE: 'string';

// Types
TYPE_ANNOTATION: '<' IDENTIFIER '>';

// Operators
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
ASSIGN: '=';
LE: '<=';
GE: '>=';
EQ: '==';
NE: '!=';
LT: '<';
GT: '>';

// Delimiters
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
COLON: ':';
COMMA: ','; 

// Literals
NUMBER: [0-9]+;
STRING: '"' (~["\\] | '\\' .)* '"';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Comments
COMMENT: '#' ~[\r\n]* -> skip;

// Whitespace
WS: [ \t\r\n]+ -> skip;
