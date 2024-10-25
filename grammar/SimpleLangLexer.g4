// Lexer grammar
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

// Indentation handling
INDENT: '    ' | '\t';
NEWLINE: '\r'? '\n';

// Identifiers and Literals
ID: [a-zA-Z_][a-zA-Z_0-9]*;
NUMBER: [0-9]+;

// Skip other whitespace
WS: [ \t]+ -> skip;
