parser grammar MyParser;

options { tokenVocab=MyLexer; }

prog: statement*;

statement: assignment SEMI? 
         | function_def SEMI?
         | func_call SEMI?;

assignment: ID TYPE? ASSIGN expression;

function_def: DEF ID LPAREN params RPAREN TYPE? LBRACE statement* RBRACE;

params: ID TYPE (COMMA ID TYPE)*;

func_call: ID LPAREN args RPAREN;

args: expression (COMMA expression)*;

expression: list_expression 
          | term ((PLUS | MINUS) term)?;

list_expression: LBRACKET (expression (COMMA expression)*)? RBRACKET;

term: factor ((MUL | DIV | MOD) factor)*;

factor: NUMBER 
      | STRING
      | ID 
      | LPAREN expression RPAREN 
      | MINUS factor // Allow for unary minus
      ;
