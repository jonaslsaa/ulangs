// Parser grammar
parser grammar SimpleLangParser;
options { tokenVocab=SimpleLangLexer; }

program: (statement NEWLINE*)* EOF;

statement
    : function
    | assignment
    | indentedStatement
    | methodCall
    | functionCall    // Add this line
    ;

function: DEF ID LPAREN params? RPAREN COLON NEWLINE functionBody;

functionBody: indentedStatement+;

indentedStatement: INDENT (returnStatement | assignment | methodCall);

params: ID (COMMA ID)*;

assignment: ID ASSIGN expression;

returnStatement: RET expression;

methodCall: ID DOT ID LPAREN args? RPAREN;

expression
    : term (PLUS term)*
    | listLiteral
    | methodCall
    | functionCall
    ;

term
    : ID
    | NUMBER
    ;

listLiteral: LBRACKET RBRACKET;

args: expression (COMMA expression)*;

functionCall: ID LPAREN args? RPAREN;