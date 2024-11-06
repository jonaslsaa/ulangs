import { DiffCodeEditor } from "../diff-editor";
import { loadOpenAIEnvVars } from "../utils";

async function testEditParser() {
  const source = `
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
CREATE: 'CREATE';
TABLE: 'TABLE';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
VARCHAR: 'VARCHAR';
INT: 'INT';
DATE: 'DATE';
PRIMARY: 'PRIMARY';
KEY: 'KEY';
FOREIGN: 'FOREIGN';
REFERENCES: 'REFERENCES';
AUTO_INCREMENT: 'AUTO_INCREMENT';
NOT: 'NOT';
NULL: 'NULL';

// Symbols
STAR: '*';
DOT: '.';
COMMA: ',';
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
EQ: '=';
NEQ: '!=';
LT: '<';
GT: '>';
LTE: '<=';
GTE: '>=';

// Literals
STRING_LITERAL: '\\'' (~'\\'')* '\\'';
DATE_LITERAL: '\\'' DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT '\\'';
NUMBER: DIGIT+;
fragment DIGIT: [0-9];

// Identifiers
IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;

// Whitespace
WS: [ \\t\\r\
]+ -> skip;
`.trim();

  const files = [{
    path: 'lexer.g4',
    content: source
  }];
  const editor = new DiffCodeEditor(loadOpenAIEnvVars());
  editor.doLogging = true;

  const mockResponse = `
The errors indicate that we need to define the tokens AVG, COUNT, AS, DELETE, GROUP, and BY in the lexer grammar. Let's add these tokens to the lexer grammar.

lexer.g4
\`\`\`antlr
<<<<<<< SEARCH
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
CREATE: 'CREATE';
TABLE: 'TABLE';
=======
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
CREATE: 'CREATE';
TABLE: 'TABLE';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
>>>>>>> REPLACE
\`\`\`

This change adds all the missing token definitions to the lexer grammar. The tokens are:
1. DELETE - for DELETE statements
2. GROUP - for GROUP BY clauses
3. BY - for GROUP BY clauses
4. AVG - for average aggregate function
5. COUNT - for count aggregate function
6. AS - for column aliases

Now all tokens used in the parser grammar are properly defined in the lexer grammar, which should resolve the warnings.
`.trim();

  const edits = editor.parseEdits(files, mockResponse);
  console.log(edits.length, "edits generated");
  console.log(edits);

  // Apply the edits
  const results = editor.applyEdits(files, edits);
  console.log("Results:");
  results.forEach(({path, content}) => {
    console.log(`<${path}>\n${content.trim()}\n</${path}>\n`);
  });
}



function test2() {
  const source = `parser grammar MyParser;
options { tokenVocab=MyLexer; }

// Start rule
program
    : statement* EOF
    ;

statement
    : selectStatement
    | updateStatement
    | insertStatement
    | createTableStatement
    | deleteStatement
    ;

createTableStatement
    : CREATE TABLE tableName LPAREN columnDefinition (COMMA columnDefinition)* RPAREN SEMI
    ;

columnDefinition
    : columnName dataType constraints?
    ;

dataType
    : INT
    | VARCHAR LPAREN DECIMAL_LITERAL RPAREN
    | DATE
    | DECIMAL LPAREN DECIMAL_LITERAL COMMA DECIMAL_LITERAL RPAREN
    ;

constraints
    : primaryKeyConstraint
    | foreignKeyConstraint
    | notNullConstraint
    | autoIncrementConstraint
    ;

primaryKeyConstraint
    : PRIMARY KEY
    ;

foreignKeyConstraint
    : FOREIGN KEY REFERENCES tableName LPAREN columnName RPAREN
    ;

notNullConstraint
    : NOT NULL
    ;

autoIncrementConstraint
    : AUTO_INCREMENT
    ;

selectStatement
    : SELECT columnList FROM tableReference (joinClause)* (whereClause)? (groupByClause)? SEMI
    ;

deleteStatement
    : DELETE FROM tableName (whereClause)? SEMI
    ;

updateStatement
    : UPDATE tableName SET setClause (whereClause)? SEMI
    ;

insertStatement
    : INSERT INTO tableName columnNames? VALUES LPAREN valueList RPAREN SEMI
    ;

columnList
    : STAR
    | columnReference (COMMA columnReference)*
    ;

columnReference
    : tableName DOT columnName
    | columnName
    ;

tableReference
    : tableName
    ;

joinClause
    : JOIN tableName ON condition
    | JOIN tableName ON columnReference EQ columnReference
    ;

whereClause
    : WHERE condition
    ;

groupByClause
    : GROUP BY columnReference
    ;

condition
    : columnReference EQ columnReference
    | columnReference EQ value
    | columnReference LT value
    | columnReference GT value
    ;

setClause
    : columnName EQ value
    ;

columnNames
    : LPAREN columnName (COMMA columnName)* RPAREN
    ;

valueList
    : value (COMMA value)*
    ;

value
    : STRING_LITERAL
    | DATE_LITERAL
    | DECIMAL_LITERAL
    | IDENTIFIER
    ;

tableName
    : IDENTIFIER
    ;

columnName
    : IDENTIFIER
    ;
`.trim();

  const files = [{
    path: 'lexer.g4',
    content: source
  }];
  const editor = new DiffCodeEditor(loadOpenAIEnvVars());
  editor.doLogging = true;

  const mockResponse = `Here are the *SEARCH/REPLACE* blocks to fix the parser grammar:

lexer.g4
\`\`\`antlr
<<<<<<< SEARCH
// Symbols
STAR: '*';
DOT: '.';
COMMA: ',';
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
EQ: '=';
LT: '<';
GT: '>';
LTE: '<=';
GTE: '>=';
NEQ: '!=';
=======
// Symbols
STAR: '*';
DOT: '.';
COMMA: ',';
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
EQ: '=';
LT: '<';
GT: '>';
LTE: '<=';
GTE: '>=';
NEQ: '!=';
BY: 'BY';
GROUP: 'GROUP';
=======
\`\`\`

parser.g4
\`\`\`antlr
<<<<<<< SEARCH
groupByClause
    : 'GROUP' BY columnList
    ;

=======
groupByClause
    : GROUP BY columnList
    ;

>>>>>>> REPLACE
\`\`\`

The changes address the issues by:
1. Adding explicit tokens for 'GROUP' and 'BY' in the lexer
2. Replacing string literals with the new tokens in the parser

This should resolve the token definition errors and allow parsing of the \`SELECT * FROM Employees;\` statement.
`.trim();

  const edits = editor.parseEdits(files, mockResponse);
  console.log(edits.length, "edits generated");
  console.log(edits);
}

test2();