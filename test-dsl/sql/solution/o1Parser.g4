parser grammar SQLParser;

options {
    tokenVocab=SQLLexer; // Use tokens defined in SQLLexer
}

// ----------------------
//        ENTRY POINT
// ----------------------
program
    : statement+ EOF
    ;

// -----------------------------
//   STATEMENTS
// -----------------------------
statement
    : createTableStmt SEMI
    | insertStmt SEMI
    | selectStmt SEMI
    | updateStmt SEMI
    | deleteStmt SEMI
    ;

// -----------------------------
//   CREATE TABLE
// -----------------------------
createTableStmt
    : CREATE TABLE tableName LPAREN tableElement (COMMA tableElement)* RPAREN
    ;

tableElement
    : createColumnDef
    | tableConstraint
    ;

createColumnDef
    : columnName typeDef columnConstraints*
    ;

typeDef
    : INT
    | DECIMAL LPAREN INT_LITERAL COMMA INT_LITERAL RPAREN
    | VARCHAR LPAREN INT_LITERAL RPAREN
    | DATE
    ;

columnConstraints
    : PRIMARY KEY
    | NOT NULL_
    | AUTO_INCREMENT
    ;

tableConstraint
    : FOREIGN KEY LPAREN columnNameList RPAREN REFERENCES tableName LPAREN columnNameList RPAREN
    ;

// -----------------------------
//   INSERT
// -----------------------------
insertStmt
    : INSERT INTO tableName LPAREN columnNameList RPAREN VALUES multiValueList
    ;

columnNameList
    : columnName (COMMA columnName)*
    ;

// multiple parentheses groups
multiValueList
    : LPAREN valueList RPAREN (COMMA LPAREN valueList RPAREN)*
    ;

valueList
    : value (COMMA value)*
    ;

// -----------------------------
//   SELECT
// -----------------------------
selectStmt
    : SELECT distinctClause? selectList FROM tableName (joinClause)* (whereClause)? (groupByClause)? (orderByClause)?
    ;

/*
  Optionally allow "DISTINCT".
  e.g. SELECT DISTINCT *
*/
distinctClause
    : DISTINCT
    ;

selectList
    : selectItem (COMMA selectItem)*
    ;

/*
  We explicitly allow the '*' token as a selectItem
  or expression (with optional AS alias).
  e.g. SELECT *, SELECT table.*, SELECT expression AS alias
*/
selectItem
    : STAR
    | expression (AS? ID)?
    ;

joinClause
    : JOIN tableName ON condition
    ;

whereClause
    : WHERE condition
    ;

groupByClause
    : GROUP BY qualifiedNameList
    ;

orderByClause
    : ORDER BY qualifiedNameList
    ;

qualifiedNameList
    : qualifiedName (COMMA qualifiedName)*
    ;

// -----------------------------
//   UPDATE
// -----------------------------
updateStmt
    : UPDATE tableName SET assignmentList (whereClause)?
    ;

assignmentList
    : assignment (COMMA assignment)*
    ;

assignment
    : columnName EQ value
    ;

// -----------------------------
//   DELETE
// -----------------------------
deleteStmt
    : DELETE FROM tableName (whereClause)?
    ;

// -----------------------------
//   EXPRESSIONS & CONDITIONS
// -----------------------------
expression
    : functionCall
    | qualifiedName
    | value
    | LPAREN expression RPAREN
    ;

functionCall
    : ID LPAREN expressionList? RPAREN
    ;

expressionList
    : expression (COMMA expression)*
    ;

/*
  Allows chaining conditions with AND/OR (basic precedence).
  Example: WHERE book_id = 1 AND member_id = 1
*/
condition
    : orCondition
    ;

orCondition
    : andCondition (OR andCondition)*
    ;

andCondition
    : notCondition (AND notCondition)*
    ;

notCondition
    : NOT? compareCondition
    ;

compareCondition
    : LPAREN condition RPAREN
    | expression (comparisonOperator expression)?
    ;

comparisonOperator
    : EQ
    | LT
    | GT
    // Extend as needed: >=, <=, <>, !=, etc.
    ;

// -----------------------------
//   HELPERS
// -----------------------------
tableName
    : ID
    ;

columnName
    : ID
    ;

qualifiedName
    : ID (DOT ID)*
    ;

value
    : INT_LITERAL
    | DEC_LITERAL
    | STRING_LITERAL
    | NULL_
    ;
