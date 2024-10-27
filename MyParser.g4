parser grammar MyParser;

options { tokenVocab=MyLexer; }

// The starting rule for parsing
program: functionDef functionCall;

// Rule to define a function definition
functionDef: DEF ID LPAREN parameterList RPAREN COLON statementBlock;

// Rule to define a list of parameters in a function
parameterList: ID (COMMA ID)*;

// Rule to define a block of statements
statementBlock: statement+;

// Rule to define an individual statement
statement: sideEffect | returnStatement;

// Rule to define a side effect
sideEffect: ID LPAREN argumentList RPAREN;

// Rule to define a return statement
returnStatement: RET ID PLUS ID;

// Rule to define a function call
functionCall: ID LPAREN argumentList RPAREN;

// Rule to define a list of arguments
argumentList: expression (COMMA expression)*;

// Rule to define an expression (for simplicity, we just allow ID and NUMBER)
expression: ID | NUMBER;
