parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : statement*
    ;

statement
    : functionDef
    | assignment
    | expression
    | forLoop
    | ifStatement
    | returnStatement
    | printStatement
    ;

functionDef
    : DEF IDENTIFIER LPAREN paramList? RPAREN typeAnnotation? COLON block
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER typeAnnotation?
    ;

typeAnnotation
    : LANGLE type RANGLE
    ;

type
    : INT_TYPE
    | STRING_TYPE
    | LIST_TYPE LANGLE type RANGLE
    | IDENTIFIER
    ;

block
    : statement+
    ;

assignment
    : IDENTIFIER typeAnnotation? ASSIGN expression
    ;

forLoop
    : FOR IDENTIFIER typeAnnotation? IN expression COLON block
    ;

ifStatement
    : IF expression COLON block (ELSE COLON block)?
    ;

returnStatement
    : RET typeAnnotation? expression?
    ;

printStatement
    : PRINT LPAREN expression RPAREN
    ;

expression
    : primary
    | expression operator expression
    | MINUS expression
    | functionCall
    | listLiteral
    ;

primary
    : INTEGER
    | STRING
    | IDENTIFIER
    | LPAREN expression RPAREN
    ;

operator
    : PLUS | MINUS | MULT | DIV | MOD | LE
    ;

functionCall
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

listLiteral
    : LBRACK (expression (COMMA expression)*)? RBRACK
    ;