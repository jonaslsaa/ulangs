lexer grammar MyLexer;

// Keywords
SELECT: 'SELECT';
FROM: 'FROM';
WHERE: 'WHERE';
JOIN: 'JOIN';
ON: 'ON';
UPDATE: 'UPDATE';
SET: 'SET';
INSERT: 'INSERT';
INTO: 'INTO';
VALUES: 'VALUES';

// Symbols
STAR: '*';
DOT: '.';
COMMA: ',';
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
EQ: '=';

// Literals
STRING_LITERAL: '\'' (~'\'')* '\'';
DATE_LITERAL: '\'' DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT '\'';
fragment DIGIT: [0-9];

// Identifiers
IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;

// Whitespace
WS: [ \t\r\n]+ -> skip;
