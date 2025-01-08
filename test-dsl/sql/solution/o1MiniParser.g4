parser grammar MyParser;

options { tokenVocab=MyLexer; }

program: statement* EOF;

statement
    : createTable
    | insert
    | select
    | update
    | delete
    | ';' // Allow empty statements
    ;

createTable
    : CREATE TABLE tableName LPAREN columnDef (COMMA columnDef)* (COMMA tableConstraint)* RPAREN SEMICOLON
    ;

tableConstraint
    : PRIMARY KEY LPAREN columnName (COMMA columnName)* RPAREN
    | FOREIGN KEY LPAREN columnName RPAREN REFERENCES tableName LPAREN columnName RPAREN
    ;

columnDef
    : columnName dataType columnConstraint*
    ;

columnConstraint
    : PRIMARY KEY
    | AUTO_INCREMENT
    | NOT NULL
    ;

insert
    : INSERT INTO tableName LPAREN columnName (COMMA columnName)* RPAREN VALUES valueSets SEMICOLON
    ;

valueSets
    : valueSet (COMMA valueSet)*
    ;

valueSet
    : LPAREN value (COMMA value)* RPAREN
    ;

select
    : SELECT (DISTINCT)? selectElements FROM tableSources (WHERE whereCondition)? (GROUP BY groupByElements)? SEMICOLON
    ;

selectElements
    : STAR
    | selectElement (COMMA selectElement)*
    ;

selectElement
    : (tableName DOT)? columnName (AS IDENTIFIER)?
    | functionCall (AS IDENTIFIER)?
    ;

functionCall
    : AVG LPAREN (tableName DOT)? columnName RPAREN
    ;

tableSources
    : tableSource (JOIN tableSource ON joinCondition)*
    ;

tableSource
    : tableName
    ;

joinCondition
    : (tableName DOT columnName) EQUAL (tableName DOT columnName)
    ;

whereCondition
    : expression
    ;

groupByElements
    : (tableName DOT)? columnName (COMMA (tableName DOT)? columnName)*
    ;

update
    : UPDATE tableName SET setClause (COMMA setClause)* (WHERE whereCondition)? SEMICOLON
    ;

setClause
    : (tableName DOT)? columnName EQUAL value
    ;

delete
    : DELETE FROM tableName (WHERE whereCondition)? SEMICOLON
    ;

value
    : STRING
    | NUMBER
    | NULL
    | IDENTIFIER
    ;

dataType
    : INT
    | VARCHAR LPAREN NUMBER RPAREN
    | DATE
    | DECIMAL LPAREN NUMBER COMMA NUMBER RPAREN
    ;

tableName
    : IDENTIFIER
    ;

columnName
    : IDENTIFIER
    ;

expression
    : expression AND expression    # AndExpression
    | expression OR expression     # OrExpression
    | NOT expression               # NotExpression
    | predicate                    # PredicateExpression
    | LPAREN expression RPAREN     # ParenthesizedExpression
    ;

predicate
    : (tableName DOT)? columnName comparisonOperator value
    ;

comparisonOperator
    : EQUAL
    | NEQ
    | LT
    | GT
    | LE
    | GE
    ;
