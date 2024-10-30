parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : statement*
    ;

statement
    : functionDef
    | expression COMMENT?
    | forLoop
    | ifStatement
    | assignment
    | returnStatement
    | PRINT LPAREN expression RPAREN
    ;

functionDef
    : DEF IDENTIFIER LPAREN paramList? RPAREN typeAnnotation? COLON block
    ;

block
    : statement+
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER typeAnnotation?
    ;

typeAnnotation
    : LT typeSpec GT
    ;

typeSpec
    : INT
    | STRING_TYPE
    | LIST typeAnnotation
    | IDENTIFIER
    ;

forLoop
    : FOR IDENTIFIER typeAnnotation? IN expression COLON block
    ;

ifStatement
    : IF expression COLON block (ELSE COLON block)?
    ;

assignment
    : IDENTIFIER typeAnnotation? ASSIGN expression
    ;

returnStatement
    : RET typeAnnotation? expression?
    ;

expression
    : LPAREN expression RPAREN                           #parenExpr
    | MINUS expression                                   #unaryMinusExpr
    | expression (MULT | DIV | MOD) expression          #multDivModExpr
    | expression (PLUS | MINUS) expression              #addSubExpr
    | expression (LT | GT | LE | GE | EQ | NE) expression #compareExpr
    | list                                              #listExpr
    | functionCall                                      #funcCallExpr
    | atom                                              #atomExpr
    ;

functionCall
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

list
    : LBRACK (expression (COMMA expression)*)? RBRACK
    ;

atom
    : NUMBER
    | STRING
    | IDENTIFIER
    ;
