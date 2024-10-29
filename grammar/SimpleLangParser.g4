parser grammar SimpleLangParser;
options { tokenVocab=SimpleLangLexer; }

program: (forStmt | printStmt)*;

forStmt: FOR ID LT INT GT IN ID COLON block;

block: (printStmt)*;

printStmt: PRINT LPAREN expr RPAREN;

expr: ID | NUMBER (PLUS NUMBER)*;
