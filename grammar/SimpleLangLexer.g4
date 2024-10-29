lexer grammar SimpleLangLexer;

FOR: 'for';
IN: 'in';
INT: 'int';
PRINT: 'print';
LPAREN: '(';
RPAREN: ')';
NUMBER: [0-9]+;
PLUS: '+';
COLON: ':';
LT: '<';
GT: '>';
WS: [ \t\r\n]+ -> skip;
ID: [a-zA-Z_][a-zA-Z_0-9]*;
