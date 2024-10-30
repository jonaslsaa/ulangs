parser grammar MyParser;

options { tokenVocab = MyLexer; }

program: functionDeclaration* ;

functionDeclaration: typeSpecifier IDENTIFIER LPAREN parameterList? RPAREN COLON statement* ;

typeSpecifier: INT | STRING_TYPE;

parameterList: parameter (COMMA parameter)* ;
parameter: typeSpecifier IDENTIFIER ;

statement: returnStatement | expressionStatement;

returnStatement: RET expression ;

expressionStatement: expression ;

expression: MINUS expression
    | expression (MULT | DIV | MOD) expression
    | expression (PLUS | MINUS) expression
    | LPAREN expression RPAREN
    | IDENTIFIER
    | NUMBER
    ;
