parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : statement*
    ;

statement
    : nodeDefinition
    | relationshipDefinition
    | outputStatement
    | conditionalStatement
    | variableAssignment
    ;

nodeDefinition
    : LPAREN nodeLabel RPAREN LBRACK propertyList RBRACK
    ;

relationshipDefinition
    : nodePattern relationshipPattern nodePattern
    ;

nodePattern
    : nodeDefinition
    | VARIABLE
    | LPAREN nodeLabel RPAREN
    ;

relationshipPattern
    : REL_START IDENTIFIER REL_END
    | ARROW
    ;

nodeLabel
    : IDENTIFIER
    ;

propertyList
    : property (COMMA property)*
    ;

property
    : IDENTIFIER EQUALS propertyValue
    ;

propertyValue
    : STRING
    | NUMBER
    ;

outputStatement
    : OUTPUT LPAREN outputArgList RPAREN
    ;

outputArgList
    : outputArg (COMMA outputArg)*
    ;

outputArg
    : propertyAccess
    | VARIABLE
    ;

propertyAccess
    : (VARIABLE | nodePattern) DOT IDENTIFIER
    ;

conditionalStatement
    : IF condition
    ;

condition
    : propertyAccess GT NUMBER
    ;

variableAssignment
    : VARIABLE EQUALS variableList
    ;

variableList
    : VARIABLE (COMMA VARIABLE)*
    ;
