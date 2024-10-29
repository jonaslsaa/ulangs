lexer grammar MyLexer;

// Whitespace
WS: [ \t\r\n]+ -> skip;

// Keywords
PRINT: 'print';
FOR: 'for';
IN: 'in';
DEF: 'def';
RET: 'ret';

// Identifiers
ID: [a-zA-Z_] [a-zA-Z0-9_]*;

// Type annotations
TYPE: '<' ID '>';

// Literals
NUMBER: [0-9]+;
STRING: '"' .*? '"' ;

// Operators and punctuation
PLUS: '+' ;
MINUS: '-' ;
MUL: '*' ;
DIV: '/' ;
MOD: '%' ;
ASSIGN: '=' ;
LPAREN: '(' ;
RPAREN: ')' ;
LBRACKET: '[' ;
RBRACKET: ']' ;
COLON: ':' ;
COMMA: ',' ;
SEMI: ';' ;
LBRACE: '{';
RBRACE: '}';

