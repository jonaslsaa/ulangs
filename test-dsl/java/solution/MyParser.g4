// MyParser.g4
parser grammar MyParser;

options { tokenVocab=MyLexer; }

program
    : classDeclaration+ EOF
    ;

classDeclaration
    : PUBLIC CLASS ID LBRACE classBody RBRACE
    ;

classBody
    : classMember*
    ;

classMember
    : modifiers memberDeclaration
    ;

modifiers
    : (PUBLIC | PRIVATE | STATIC)*
    ;

memberDeclaration
    : constructorDeclaration
    | methodDeclaration
    | fieldDeclaration
    ;

fieldDeclaration
    : type ID (ASSIGN expression)? SEMI
    ;

methodDeclaration
    : type ID LPAREN parameterList? RPAREN block
    ;

constructorDeclaration
    : ID LPAREN parameterList? RPAREN block
    ;

parameterList
    : parameter (COMMA parameter)*
    ;

parameter
    : type ID
    ;

type
    : (INT | DOUBLE | STRING | VOID | ID) (LBRACK RBRACK)*
    ;

block
    : LBRACE statement* RBRACE
    ;

statement
    : variableDeclaration SEMI
    | assignment SEMI
    | expressionStatement SEMI
    | ifStatement
    | forStatement
    | returnStatement SEMI
    | block
    ;

variableDeclaration
    : type ID (ASSIGN expression)?
    ;

assignment
    : target ASSIGN expression
    | target assignmentOperator expression
    ;

target
    : ID
    | memberAccessTarget
    ;

memberAccessTarget
    : (ID | THIS) (DOT ID)*
    ;

assignmentOperator
    : PLUS_ASSIGN
    | MINUS_ASSIGN
    | MULT_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    ;

expressionStatement
    : expression
    ;

ifStatement
    : IF LPAREN expression RPAREN statement (ELSE statement)?
    ;

forStatement
    : FOR LPAREN forTraditional SEMI expression? SEMI forUpdate RPAREN statement
    | FOR LPAREN forEnhanced RPAREN statement
    ;

forTraditional
    : forInit
    ;

forEnhanced
    : type ID COLON expression
    ;

forInit
    : variableDeclaration
    | assignment
    ;

forUpdate
    : expression (COMMA expression)*
    ;

returnStatement
    : RETURN expression
    ;

expression
    : logicalOrExpr
    ;

logicalOrExpr
    : logicalAndExpr (OR logicalAndExpr)*
    ;

logicalAndExpr
    : equalityExpr (AND equalityExpr)*
    ;

equalityExpr
    : relationalExpr ((EQ | NEQ) relationalExpr)*
    ;

relationalExpr
    : additiveExpr ((LT | GT | LE | GE) additiveExpr)*
    ;

additiveExpr
    : multiplicativeExpr ((PLUS | MINUS) multiplicativeExpr)*
    ;

multiplicativeExpr
    : unaryExpr ((STAR | DIV | MOD) unaryExpr)*
    ;

unaryExpr
    : (NOT | PLUS | MINUS)? castExpr
    ;

castExpr
    : LPAREN type RPAREN unaryExpr
    | postfixExpr
    ;

postfixExpr
    : primaryExpr (INC | DEC)*
    ;

primaryExpr
    : NEW type LPAREN argumentList? RPAREN memberAccess*
    | arrayInitializer
    | LPAREN expression RPAREN
    | methodCall memberAccess*
    | literal
    | THIS memberAccess*
    | ID memberAccess*
    ;

memberAccess
    : DOT ID LPAREN argumentList? RPAREN  // Method call after dot
    | DOT ID                             // Property access after dot
    ;

methodCall
    : qualifiedName LPAREN argumentList? RPAREN
    ;

qualifiedName
    : ID (DOT ID)*
    ;

argumentList
    : expression (COMMA expression)*
    ;

arrayInitializer
    : LBRACE expressionList? RBRACE
    ;

expressionList
    : expression (COMMA expression)*
    ;

literal
    : INT_LITERAL
    | DOUBLE_LITERAL
    | STRING_LITERAL
    ;
