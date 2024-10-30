lexer grammar MyLexer;

// Keywords
START: 'Start';
END: 'End';
IF: 'If';
ELSE: 'Else';
FOREACH: 'ForEach';
IN: 'in';
USING: 'using';
WITH: 'with';
AS: 'as';
STOP: 'Stop';
BY: 'by';
DATABASE: 'Database';
API: 'API';

// Commands
NOTIFY: 'Notify';
TRANSFORM: 'Transform';
AGGREGATE: 'Aggregate';
REPORT: 'Report';
GENERATE: 'Generate';
ARCHIVE: 'Archive';
LOG: 'Log';
APPEND: 'Append';
TO: 'to';
CATCH: 'Catch';
ERRORTYPE: 'ErrorType';
FILE: 'File';
SAVE: 'Save';
REPEAT: 'Repeat';
CONDITION: 'Condition';

// Symbols
COLON: ':';
ARROW: '->';
DOT: '.';
COMMA: ',';
GREATER: '>';
LESS: '<';
EQUAL: '=';

// Complex tokens
IDENTIFIER: [a-zA-Z][a-zA-Z0-9_]*;
STRING: '"' .*? '"';
NUMBER: [0-9]+;

// Skip whitespace and comments
WS: [ \t\r\n]+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;
