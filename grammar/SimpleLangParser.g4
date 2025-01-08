parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : (statement | functionDef)*
    ;

functionDef
    : DEF IDENTIFIER LPAREN paramList? RPAREN typeAnnotation? COLON
      block
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER typeAnnotation?
    ;

typeAnnotation
    : LT type GT
    ;

type
    : INT_TYPE
    | STRING_TYPE
    | LIST_TYPE typeAnnotation
    | IDENTIFIER
    ;

block
    : statement+
    ;

statement
    : assignment
    | forLoop
    | ifStatement
    | functionCall COMMENT?
    | returnStatement
    | printStatement
    | COMMENT
    ;

assignment
    : IDENTIFIER typeAnnotation? ASSIGN expression
    ;

forLoop
    : FOR IDENTIFIER typeAnnotation? IN expression COLON
      block
    ;

ifStatement
    : IF expression COLON
      block
      (ELSE COLON block)?
    ;

returnStatement
    : RET typeAnnotation? expression
    ;

printStatement
    : PRINT LPAREN expression RPAREN
    ;

expression
    : MINUS expression
    | expression (MULT | DIV | MOD) expression
    | expression (PLUS | MINUS) expression
    | expression LE expression
    | LPAREN expression RPAREN
    | list
    | functionCall
    | atom
    ;

functionCall
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

list
    : LBRACK (expression (COMMA expression)*)? RBRACK
    ;

atom
    : INTEGER
    | STRING
    | IDENTIFIER
    ;
