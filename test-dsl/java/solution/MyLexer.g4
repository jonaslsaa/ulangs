// MyLexer.g4
lexer grammar MyLexer;

// Keywords
PUBLIC: 'public';
PRIVATE: 'private';
CLASS: 'class';
STATIC: 'static';
VOID: 'void';
STRING: 'String';
INT: 'int';
DOUBLE: 'double';
IF: 'if';
ELSE: 'else';
FOR: 'for';
NEW: 'new';
RETURN: 'return';
THIS: 'this';

// Operators
ASSIGN: '=';
PLUS: '+';
MINUS: '-';
STAR: '*';
DIV: '/';
MOD: '%';
EQ: '==';
NEQ: '!=';
LT: '<';
GT: '>';
LE: '<=';
GE: '>=';
AND: '&&';
OR: '||';
NOT: '!';

// Assignment Operators
PLUS_ASSIGN: '+=';
MINUS_ASSIGN: '-=';
MULT_ASSIGN: '*=';
DIV_ASSIGN: '/=';
MOD_ASSIGN: '%=';

// Postfix Operators
INC: '++';
DEC: '--';

// Symbols
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
LBRACK: '[';
RBRACK: ']';
SEMI: ';';
COMMA: ',';
DOT: '.';
COLON: ':';

// Literals
INT_LITERAL: [0-9]+;
DOUBLE_LITERAL: [0-9]+ '.' [0-9]+;
STRING_LITERAL: '"' (~["\\] | '\\' .)* '"' ;

// Identifiers
ID: [a-zA-Z_][a-zA-Z_0-9]*;

// Whitespace and Comments
WS: [ \t\r\n]+ -> skip;

LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
