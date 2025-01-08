// <MyLexer.g4>
lexer grammar MyLexer;

// Keywords (Note: Removed AVG to treat it as IDENTIFIER)
SELECT          : [sS][eE][lL][eE][cC][tT];
FROM            : [fF][rR][oO][mM];
WHERE           : [wW][hH][eE][rR][eE];
JOIN            : [jJ][oO][iI][nN];
ON              : [oO][nN];
GROUP           : [gG][rR][oO][uU][pP];
BY              : [bB][yY];
CREATE          : [cC][rR][eE][aA][tT][eE];
TABLE           : [tT][aA][bB][lL][eE];
INSERT          : [iI][nN][sS][eE][rR][tT];
INTO            : [iI][nN][tT][oO];
VALUES          : [vV][aA][lL][uU][eE][sS];
UPDATE          : [uU][pP][dD][aA][tT][eE];
SET             : [sS][eE][tT];
DELETE          : [dD][eE][lL][eE][tT][eE];
PRIMARY         : [pP][rR][iI][mM][aA][rR][yY];
KEY             : [kK][eE][yY];
FOREIGN         : [fF][oO][rR][eE][iI][gG][nN];
REFERENCES      : [rR][eE][fF][eE][rR][eE][nN][cC][eE][sS];
AUTO_INCREMENT  : [aA][uU][tT][oO] '_' [iI][nN][cC][rR][eE][mM][eE][nN][tT];
NOT             : [nN][oO][tT];
NULL            : [nN][uU][lL][lL];
AS              : [aA][sS];
DISTINCT        : [dD][iI][sS][tT][iI][nN][cC][tT];
AND             : [aA][nN][dD];
OR              : [oO][rR];

// Symbols
STAR            : '*';
COMMA           : ',';
SEMICOLON       : ';';
LPAREN          : '(';
RPAREN          : ')';
EQ              : '=';
DOT             : '.';
LT              : '<';
GT              : '>';
LE              : '<=';
GE              : '>=';
NEQ             : '<>' | '!=';

// Operators
PLUS            : '+';
MINUS           : '-';
DIV             : '/';

// Data Types
INT             : [iI][nN][tT];
VARCHAR         : [vV][aA][rR][cC][hH][aA][rR];
DATE            : [dD][aA][tT][eE];
DECIMAL         : [dD][eE][cC][iI][mM][aA][lL];

// Literals
NUMBER          : [0-9]+ ('.' [0-9]+)?;
STRING_LITERAL  : '\'' (~['\r\n])* '\'';

// Identifiers
IDENTIFIER      : [a-zA-Z_][a-zA-Z0-9_]*;

// Comments and Whitespace
COMMENT         : '--' ~[\r\n]* -> skip;
WS              : [ \t\r\n]+ -> skip;
