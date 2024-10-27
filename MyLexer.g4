lexer grammar MyLexer;

// Token for the "def" keyword for function definitions
DEF: 'def';

// Token for the "ret" keyword for return statements
RET: 'ret';

// Token for the "print" function (not used in this snippet but included for completeness)
PRINT: 'print';

// Token for numeric literals (positive integers)
NUMBER: [0-9]+;

// Token for identifiers (variable names)
ID: [a-zA-Z_][a-zA-Z_0-9]*;

// Token for the less than operator (not used in this snippet)
LT: '<';

// Token for the greater than operator (not used in this snippet)
GT: '>';

// Token for the comma character
COMMA: ',';

// Token for parentheses
LPAREN: '(';
RPAREN: ')';

// Token for whitespace, which is ignored
WS: [ \t\r\n]+ -> skip;

// Token for the plus operator
PLUS: '+';

// Token for the colon character
COLON: ':';
