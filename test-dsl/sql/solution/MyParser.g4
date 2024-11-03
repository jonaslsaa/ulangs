parser grammar MyParser;
options { tokenVocab=MyLexer; }

// Start rule
program
    : statement* EOF
    ;

statement
    : selectStatement
    | updateStatement
    | insertStatement
    ;

selectStatement
    : SELECT columnList FROM tableReference (joinClause)? (whereClause)? SEMI
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
    : tableName DOT columnName // Handle fully qualified column name (table.column)
    | columnName // Handle standalone column name
    ;

tableReference
    : tableName
    ;

joinClause
    : JOIN tableName ON condition
    ;

whereClause
    : WHERE condition
    ;

condition
    : columnReference EQ value
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
    | IDENTIFIER
    ;

tableName
    : IDENTIFIER
    ;

columnName
    : IDENTIFIER
    ;
