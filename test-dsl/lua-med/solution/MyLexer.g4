lexer grammar MyLexer;

// New rule to skip Lua multiline comments.
ML_COMMENT
    : '--[[' .*? ']]' -> skip;

// Single-line comment rule.
COMMENT
    : '--' ~[\r\n]* -> skip;

PRINT       : 'print';
IF          : 'if';
THEN        : 'then';
ELSE        : 'else';
ELSEIF      : 'elseif';
END         : 'end';
WHILE       : 'while';
DO          : 'do';
REPEAT      : 'repeat';
UNTIL       : 'until';
FOR         : 'for';
IN          : 'in';
FUNCTION    : 'function';
LOCAL       : 'local';
RETURN      : 'return';
BREAK       : 'break';
NIL         : 'nil';
TRUE        : 'true';
FALSE       : 'false';
AND         : 'and';
OR          : 'or';
NOT         : 'not';

ASSIGN      : '=';
PLUS        : '+';
MINUS       : '-';
MULT        : '*';
DIV         : '/';
MOD         : '%';
POW         : '^';
CONCAT      : '..';

LT          : '<';
GT          : '>';
LE          : '<=';
GE          : '>=';
EQ          : '==';
NEQ         : '~=';

LENGTH      : '#' ;

LPAREN      : '(';
RPAREN      : ')';
LBRACE      : '{';
RBRACE      : '}';
LBRACK      : '[';
RBRACK      : ']';
COMMA       : ',';
SEMI        : ';';
COLON       : ':';
DOT         : '.';

ID          : [a-zA-Z_][a-zA-Z0-9_]*;
NUMBER      : [0-9]+ ('.' [0-9]+)?;
STRING
    : '"' ( ~["\\] | '\\' . )* '"'
    | '\'' ( ~['\\] | '\\' . )* '\''
    ;

WS          : [ \t\r\n]+ -> skip;
