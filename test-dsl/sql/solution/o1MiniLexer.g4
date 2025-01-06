lexer grammar MyLexer;

// Keywords (case-insensitive)
CREATE: [Cc][Rr][Ee][Aa][Tt][Ee];
TABLE: [Tt][Aa][Bb][Ll][Ee];
PRIMARY: [Pp][Rr][Ii][Mm][Aa][Rr][Yy];
KEY: [Kk][Ee][Yy];
AUTO_INCREMENT: [Aa][Uu][Tt][Oo] '_' [Ii][Nn][Cc][Rr][Ee][Mm][Ee][Nn][Tt];
NOT: [Nn][Oo][Tt];
NULL: [Nn][Uu][Ll][Ll];
INSERT: [Ii][Nn][Ss][Ee][Rr][Tt];
INTO: [Ii][Nn][Tt][Oo];
VALUES: [Vv][Aa][Ll][Uu][Ee][Ss];
SELECT: [Ss][Ee][Ll][Ee][Cc][Tt];
FROM: [Ff][Rr][Oo][Mm];
JOIN: [Jj][Oo][Ii][Nn];
ON: [Oo][Nn];
UPDATE: [Uu][Pp][Dd][Aa][Tt][Ee];
SET: [Ss][Ee][Tt];
WHERE: [Ww][Hh][Ee][Rr][Ee];
DELETE: [Dd][Ee][Ll][Ee][Tt][Ee];
FOREIGN: [Ff][Oo][Rr][Ee][Ii][Gg][Nn];
REFERENCES: [Rr][Ee][Ff][Ee][Rr][Ee][Nn][Cc][Ee][Ss];
INT: [Ii][Nn][Tt];
VARCHAR: [Vv][Aa][Rr][Cc][Hh][Aa][Rr];
DATE: [Dd][Aa][Tt][Ee];
DECIMAL: [Dd][Ee][Cc][Ii][Mm][Aa][Ll];
AS: [Aa][Ss];
DISTINCT: [Dd][Ii][Ss][Tt][Ii][Nn][Cc][Tt];
GROUP: [Gg][Rr][Oo][Uu][Pp];
BY: [Bb][Yy];
AVG: [Aa][Vv][Gg];

// Operators
EQUAL: '=';
COMMA: ',';
SEMICOLON: ';';
DOT: '.';
LPAREN: '(';
RPAREN: ')';
STAR: '*';
PLUS: '+';
MINUS: '-';
DIV: '/';
LT: '<';
GT: '>';
LE: '<=';
GE: '>=';
NEQ: '!=';
AND: [Aa][Nn][Dd];
OR: [Oo][Rr];
NOT_OP: [Nn][Oo][Tt];

// Literals
STRING: '\'' (~['\\] | '\\' .)* '\'';
NUMBER: [0-9]+ ('.' [0-9]+)?;
IDENTIFIER: [a-zA-Z_][a-zA-Z_0-9]*;

// Whitespace and Comments
WS: [ \t\r\n]+ -> skip;
COMMENT: '--' ~[\r\n]* -> skip;
