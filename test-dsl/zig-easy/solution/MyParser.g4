parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : topLevelDecl* EOF
    ;

topLevelDecl
    : importDecl
    | functionDecl
    | structDecl
    | enumDecl
    | declarationStmt
    ;

importDecl
    : CONST ID ASSIGN AT ID LPAREN STRING RPAREN SEMI
    ;

functionDecl
    : (PUB)? FN ID LPAREN paramList? RPAREN returnType? block
    ;

paramList
    : parameter (COMMA parameter)*
    ;

parameter
    : ID COLON type
    ;

// Return type already consumes an optional '!' 
returnType
    : ('!' )? type
    ;

// Revised type production allows nullable, pointer, array, and qualified types.
type
    : QUESTION? ( pointerType | arrayType | qualifiedType )
    ;

pointerType
    : STAR+ qualifiedType
    ;

arrayType
    : LSQUARE RSQUARE qualifiedType
    ;

// Revised: Allow qualifiedType to be VOID or an ID (with optional dot-qualified parts)
qualifiedType
    : (VOID | ID) (DOT (VOID | ID))*
    ;

block
    : LBRACE statement* RBRACE
    ;

statement
    : declarationStmt
    | expressionStmt
    | ifStmt
    | forStmt
    | returnStmt
    | deferStmt
    | comptimeStmt        // support compile-time blocks
    ;

comptimeStmt
    : COMPTIME block
    ;

declarationStmt
    : (CONST | VAR) ID (COLON type)? (ASSIGN expression)? SEMI
    ;

expressionStmt
    : expression SEMI
    ;

ifStmt
    : IF expression (PIPE ID PIPE)? block (ELSE block)?
    ;

forStmt
    : FOR LPAREN expression RPAREN (PIPE ID PIPE)? block
    ;

returnStmt
    : RETURN expression? SEMI
    ;

deferStmt
    : DEFER expression SEMI
    ;

expression
    : assignment
    ;

assignment
    : conditional ( (ASSIGN | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN) assignment )?
    ;

conditional
    : logicalOr
    ;

logicalOr
    : logicalAnd ( OR logicalAnd )*
    ;

logicalAnd
    : equality ( AND equality )*
    ;

equality
    : relational ( (EQUAL | NOTEQUAL) relational )*
    ;

relational
    : additive ( (LT | GT | LE | GE) additive )*
    ;

additive
    : multiplicative ( (PLUS | MINUS) multiplicative )*
    ;

multiplicative
    : unary ( (STAR | DIV | MOD) unary )*
    ;

// Updated: Include AMP and TRY in the list of unary operators.
unary
    : (PLUS | MINUS | NOT | AMP | TRY) unary
    | postfix
    ;

postfix
    : primary ( postOp )*
    ;

postOp
    : functionCall
    | memberAccess
    | arrayAccess
    ;

functionCall
    : LPAREN argumentList? RPAREN
    ;

argumentList
    : expression (COMMA expression)*
    ;

memberAccess
    : DOT ID
    ;

// Allow both indexing and slice access.
// This rule permits a single index or a slicing using either a colon or DOTDOT.
arrayAccess
    : LSQUARE expression ( (COLON | DOTDOT) expression )? RSQUARE
    ;

// Support built-in calls with '@'
builtinCall
    : AT ID functionCall
    ;

primary
    : builtinCall
    | literal
    | ID
    | LPAREN expression RPAREN
    ;

// Updated literal alternatives:
//   1. anonymousStruct – now with field specifications allowed
//   2. arrayLiteral (for compound array literals like [_]i32{...})
//   3. structInit (named struct initializer)
//   4. enumLiteral (for enum expressions, e.g., "enum { Red, Green, Blue, }")
//   5. INT, STRING, NULL, anonComposite
literal
    : anonymousStruct
    | arrayLiteral
    | structInit
    | enumLiteral
    | INT
    | STRING
    | NULL
    | anonComposite
    ;

// Revised anonymous struct literal for inline type definitions.
// It allows either method definitions or simple field specifications.
anonymousStruct
    : STRUCT LBRACE structLiteralMemberList? RBRACE
    ;

structLiteralMemberList
    : structLiteralMember (COMMA structLiteralMember)* (COMMA)?
    ;

structLiteralMember
    : functionDecl
    | fieldSpec
    ;

fieldSpec
    : ID COLON type
    ;

// Array literal compound: LSQUARE UNDERSCORE RSQUARE type LBRACE expressionList? RBRACE
arrayLiteral
    : LSQUARE UNDERSCORE RSQUARE type LBRACE expressionList? RBRACE
    ;

// Named struct initialization (e.g., Point{ .x = 10, .y = 20 })
structInit
    : ID LBRACE fieldInitList? RBRACE
    ;

// Enum literal: for expressions like "enum { Red, Green, Blue, }"
enumLiteral
    : ENUM LBRACE enumFieldList? RBRACE
    ;

anonComposite
    : DOT LBRACE expressionList? RBRACE
    ;

expressionList
    : expression (COMMA expression)*
    ;

fieldInitList
    : fieldInit (COMMA fieldInit)*
    ;

fieldInit
    : DOT ID ASSIGN expression
    ;

structDecl
    : (CONST)? STRUCT ID LBRACE structFieldList? RBRACE
    ;

structFieldList
    : structField (COMMA structField)*
    ;

structField
    : ID COLON type
    ;

// Top-level enum declaration remains unchanged.
// It is used when defining an enum type with a name, e.g.,
// "enum Color { Red, Green, Blue, }"
enumDecl
    : (CONST)? ENUM ID LBRACE enumFieldList? RBRACE
    ;

// Allow an optional trailing comma in enum field lists.
enumFieldList
    : ID (COMMA ID)* (COMMA)?
    ;
