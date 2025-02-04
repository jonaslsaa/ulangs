lexer grammar MyLexer;

UNDERSCORE  : '_' ;  // must appear before ID so that "_" is not consumed as an ID

CONST       : 'const';
PUB         : 'pub';
FN          : 'fn';
STRUCT      : 'struct';
ENUM        : 'enum';
RETURN      : 'return';
IF          : 'if';
ELSE        : 'else';
FOR         : 'for';
VAR         : 'var';
DEFER       : 'defer';
COMPTIME    : 'comptime';
TRY         : 'try';
VOID        : 'void';
NULL        : 'null';

// NEW: Token for nullable types
QUESTION    : '?' ;

// NEW: Token for address-of operator
AMP         : '&' ;

ASSIGN      : '=';
ADD_ASSIGN  : '+=';
SUB_ASSIGN  : '-=';
MUL_ASSIGN  : '*=';
DIV_ASSIGN  : '/=';

COLON       : ':';
SEMI        : ';';
COMMA       : ',';
DOT         : '.';
AT          : '@';
PIPE        : '|';

LPAREN      : '(';
RPAREN      : ')';
LBRACE      : '{';
RBRACE      : '}';
LSQUARE     : '[';
RSQUARE     : ']';

STAR        : '*';
DIV         : '/';
MOD         : '%';
PLUS        : '+';
MINUS       : '-';
NOT         : '!';
LT          : '<';
GT          : '>';
LE          : '<=';
GE          : '>=';
EQUAL       : '==';
NOTEQUAL    : '!=';

DOTDOT      : '..';
OR          : '||';
AND         : '&&';

INT         : [0-9]+;
STRING      : '"' ( ~["\\] | '\\' . )* '"';
ID          : [a-zA-Z_][a-zA-Z0-9_]*;

WS          : [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
