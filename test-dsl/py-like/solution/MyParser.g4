parser grammar MyParser;

options { tokenVocab=MyLexer; }

program:   function*;

function: DEF ID LPAREN parameterList RPAREN COLON statement+ ;

parameterList: ID (COMMA ID)*;

statement: assignment SEMI? | functionCall SEMI? | RET expression SEMI?;

assignment: ID ASSIGN expression;

functionCall: ID LPAREN argumentList RPAREN;

argumentList: expression (COMMA expression)*;

expression: additiveExpression;

additiveExpression: multiplicativeExpression ( (PLUS | MINUS) multiplicativeExpression )* ;

multiplicativeExpression: unaryExpression ( (MUL | DIV | MOD) unaryExpression )* ;


unaryExpression: (PLUS | MINUS)? primary ;


primary: ID | NUMBER | LPAREN expression RPAREN;
