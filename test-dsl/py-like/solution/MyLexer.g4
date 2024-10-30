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
LIST: 'List';

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

// Type annotations (improved whitespace handling)
TYPE_ANNOTATION: '<' WS* (INT | STRING_TYPE | LIST | IDENTIFIER) (WS* '<' WS* (INT | STRING_TYPE | LIST | IDENTIFIER) WS* '>')* WS* '>';


// Comments
SINGLE_LINE_COMMENT: '#' ~[\r\n]* -> skip;
MULTI_LINE_COMMENT: '//' ~[\r\n]* -> skip;

// Whitespace
WS: [ \t\r\n]+ -> skip;
