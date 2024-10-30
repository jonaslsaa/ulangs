parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : section+
    ;

section
    : sectionHeader statementList
    ;

sectionHeader
    : IDENTIFIER COLON
    | START COLON
    | END COLON
    ;

statementList
    : statement*
    ;

statement
    : notifyStatement
    | dataAccessStatement
    | transformStatement
    | conditionStatement
    | aggregateStatement
    | reportStatement
    | archiveStatement
    | fileStatement
    | repeatStatement
    | logStatement
    | appendStatement
    | errorHandlingStatement
    | stopStatement
    ;

notifyStatement
    : NOTIFY STRING
    ;

dataAccessStatement
    : ARROW (DATABASE|API) STRING
    ;

transformStatement
    : TRANSFORM IDENTIFIER USING STRING
    ;

conditionStatement
    : CONDITION IF expression
        statementList
      (ELSE IF expression
        statementList)*
      (ELSE
        statementList)?
    ;

aggregateStatement
    : AGGREGATE IDENTIFIER (BY IDENTIFIER)? AS IDENTIFIER
    ;

reportStatement
    : REPORT GENERATE STRING WITH IDENTIFIER
    ;

archiveStatement
    : ARCHIVE identifierList
    ;

fileStatement
    : FILE SAVE STRING WITH IDENTIFIER
    ;

repeatStatement
    : REPEAT FOREACH IDENTIFIER IN IDENTIFIER statementList
    ;

logStatement
    : LOG STRING (WITH expression)?
    ;

appendStatement
    : APPEND IDENTIFIER TO IDENTIFIER
    ;

errorHandlingStatement
    : CATCH ERRORTYPE STRING logStatement
    ;

stopStatement
    : STOP
    ;

expression
    : IDENTIFIER DOT IDENTIFIER
    | IDENTIFIER
    | NUMBER
    | STRING
    | expression GREATER expression
    | expression LESS expression
    | expression EQUAL expression
    ;

identifierList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;
