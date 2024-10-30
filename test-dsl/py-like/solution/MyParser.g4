parser grammar MyParser;
options { tokenVocab = MyLexer; }

program: (functionDeclaration | statement)* EOF;

functionDeclaration
    : DEF IDENTIFIER LPAREN parameterList? RPAREN typeAnnotation COLON statement+
    ;

parameterList: parameter (COMMA parameter)*;

parameter: IDENTIFIER typeAnnotation;

typeAnnotation
    : TYPE_ANNOTATION
    ;

statement
    : returnStatement
    | expressionStatement
    | printStatement
    | assignmentStatement
    | ifStatement
    | forStatement
    ;

printStatement: PRINT LPAREN expression RPAREN;

returnStatement: RET expression;

expressionStatement: expression;

assignmentStatement: IDENTIFIER typeAnnotation? ASSIGN expression;

ifStatement
    : IF expression COLON statement+
    (ELSE COLON statement+)?
    ;

forStatement
    : FOR IDENTIFIER typeAnnotation? IN expression COLON statement+
    ;

expression
    : MINUS expression
    | expression (MULT | DIV | MOD) expression
    | expression (PLUS | MINUS) expression
    | expression (LT | GT | LE | GE | EQ | NE) expression
    | LPAREN expression RPAREN
    | functionCall
    | list
    | IDENTIFIER
    | NUMBER
    | STRING
    ;

functionCall
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

list
    : LBRACK (expression (COMMA expression)*)? RBRACK
    ;
