parser grammar SimpleLangParser;

options {
    tokenVocab=SimpleLangLexer;
}

program: statement*;

statement: function
         | assignment
         | returnStatement
         | methodCall
         ;

function: DEF ID LPAREN params? RPAREN COLON statement;

params: ID (COMMA ID)*;

assignment: ID ASSIGN expression;

returnStatement: RET expression;

methodCall: ID DOT ID LPAREN args? RPAREN;

expression: term (PLUS term)*
          | listLiteral
          | methodCall
          | functionCall  // Added functionCall rule
          ;

term: ID
    | NUMBER
    ;

listLiteral: LBRACKET RBRACKET;

args: expression (COMMA expression)*;

functionCall: ID LPAREN args? RPAREN;  // Added functionCall rule