// <MyParser.g4>
parser grammar MyParser;

options { tokenVocab=MyLexer; }

program
    : statement+ EOF
    ;

statement
    : selectStmt SEMICOLON
    | insertStmt SEMICOLON
    | updateStmt SEMICOLON
    | deleteStmt SEMICOLON
    | createTableStmt SEMICOLON
    ;

selectStmt
    : SELECT (DISTINCT)? selectElements FROM tableReferences (whereClause)? (groupByClause)?
    ;

selectElements
    : STAR
    | selectExprList
    ;

selectExprList
    : selectExpr (COMMA selectExpr)*
    ;

selectExpr
    : expression (AS IDENTIFIER)?
    ;

tableReferences
    : tableReference (joinClause)*
    ;

tableReference
    : IDENTIFIER (AS IDENTIFIER)?
    ;

joinClause
    : JOIN tableReference ON expression
    ;

whereClause
    : WHERE expression
    ;

groupByClause
    : GROUP BY identifierList
    ;

insertStmt
    : INSERT INTO IDENTIFIER LPAREN identifierList RPAREN VALUES tupleList
    ;

tupleList
    : LPAREN valueList RPAREN (COMMA LPAREN valueList RPAREN)*
    ;

valueList
    : value (COMMA value)*
    ;

value
    : STRING_LITERAL
    | NUMBER
    | IDENTIFIER
    ;

updateStmt
    : UPDATE IDENTIFIER SET setClauseList (whereClause)?
    ;

setClauseList
    : setClause (COMMA setClause)*
    ;

setClause
    : IDENTIFIER EQ value
    ;

deleteStmt
    : DELETE FROM IDENTIFIER (whereClause)?
    ;

createTableStmt
    : CREATE TABLE IDENTIFIER LPAREN columnDefList (COMMA tableConstraint)* RPAREN
    ;

columnDefList
    : columnDef (COMMA columnDef)*
    ;

columnDef
    : IDENTIFIER dataType columnConstraint*
    ;

dataType
    : INT
    | VARCHAR LPAREN NUMBER RPAREN
    | DATE
    | DECIMAL LPAREN NUMBER COMMA NUMBER RPAREN
    ;

columnConstraint
    : PRIMARY KEY
    | FOREIGN KEY REFERENCES IDENTIFIER LPAREN IDENTIFIER RPAREN
    | NOT NULL
    | AUTO_INCREMENT
    ;

tableConstraint
    : PRIMARY KEY LPAREN identifierList RPAREN
    | FOREIGN KEY LPAREN identifierList RPAREN REFERENCES IDENTIFIER LPAREN IDENTIFIER RPAREN
    ;

identifierList
    : column (COMMA column)*
    ;

expression
    : functionCall
    | expression operator expression
    | LPAREN expression RPAREN
    | operand
    ;

functionCall
    : IDENTIFIER LPAREN argList? RPAREN
    ;

argList
    : expression (COMMA expression)*
    ;

operator
    : EQ
    | LT
    | GT
    | LE
    | GE
    | NEQ
    | PLUS
    | MINUS
    | STAR
    | DIV
    | AND
    | OR
    ;

operand
    : IDENTIFIER DOT IDENTIFIER    # QualifiedIdentifier
    | IDENTIFIER                   # SimpleIdentifier
    | STRING_LITERAL               # StringOperand
    | NUMBER                       # NumberOperand
    ;

column
    : IDENTIFIER (DOT IDENTIFIER)?
    ;
