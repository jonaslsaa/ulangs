parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : block EOF
    ;

block
    : (statement SEMI?)* 
    ;

//
// A top-level statement may be a function call statement, an expression statement,
// an assignment, or a control structure.
statement
    : functionCallStmt        // e.g., Vector.print(velocity)
    | exprStatement           // a bare expression.
    | assignment              // e.g., Vector.new = function(self, object) ... end
    | ifStatement
    | whileStatement
    | repeatStatement
    | forStatement
    | doStatement
    | returnStatement
    | breakStatement
    | localDeclaration
    | functionDeclaration
    ;

//
// Assignment: a var-list followed by '=' and an expression list.
assignment
    : varList ASSIGN expList
    ;

varList
    : var (COMMA var)*
    ;
//
// An assignment target “var” is defined as a prefix expression that does NOT include any call–suffix.
var
    : prefixExpWithoutCall
    ;

expList
    : expression (COMMA expression)*
    ;

/* Expression grammar with operator precedence */
expression
    : expression op=OR expression                     #OrExp
    | expression op=AND expression                    #AndExp
    | expression op=(EQ | NEQ | LT | LE | GT | GE) expression  #RelationalExp
    | expression op=CONCAT expression                 #ConcatExp
    | expression op=(PLUS | MINUS) expression         #AddSubExp
    | expression op=(MULT | DIV | MOD) expression     #MulDivExp
    | expression op=POW expression                    #PowExp
    | NOT expression                                  #NotExp
    | MINUS expression                                #UnaryMinusExp
    | LENGTH expression                               #LengthExp
    | prefixExp                                     #PrefixExpression
    | NUMBER                                          #NumberLiteral
    | STRING                                          #StringLiteral
    | NIL                                             #NilLiteral
    | TRUE                                            #TrueLiteral
    | FALSE                                           #FalseLiteral
    | LPAREN expression RPAREN                        #ParenExp
    | tableConstructorExp                             #TableConsExp
    | functionExp                                     #FuncExprExp
    ;

//
// A “prefix expression” without any call–suffixes (for use in assignments).
prefixExpWithoutCall
    : primary (varSuffix)*
    ;

//
// A “callable” prefix expression: one that has at least one call–suffix.
prefixExpWithCall
    : prefixExpWithoutCall (callSuffix)+ (varSuffix)*  
    ;

//
// A generic prefix expression: either with or without call–suffixes.
prefixExp
    : prefixExpWithCall
    | prefixExpWithoutCall
    ;

//
// varSuffix: supports dot and bracket forms.
varSuffix
    : DOT (ID | PRINT)
    | LBRACK expression RBRACK
    ;

//
// callSuffix: either parentheses or colon–call. Notice we now allow COLON followed by (ID | PRINT).
callSuffix
    : LPAREN expList? RPAREN
    | COLON (ID | PRINT) LPAREN expList? RPAREN
    ;

//
// Function call statement – used as a top–level statement.
functionCallStmt
    : prefixExpWithCall
    ;
    
//
// Function call used in expressions.
functionCall
    : prefixExpWithCall
    ;

//
// A bare expression statement.
exprStatement
    : expression
    ;

//
// Primary expression: the basic building block.
primary
    : (ID | PRINT)
    | NUMBER
    | STRING
    | NIL
    | TRUE
    | FALSE
    | LPAREN expression RPAREN
    | tableConstructorExp
    ;

//
// Table constructor.
tableConstructorExp
    : LBRACE fieldList? RBRACE
    ;

fieldList
    : field ( (COMMA | SEMI) field )*
    ;
field
    : (ID | PRINT) ASSIGN expression
    | LBRACK expression RBRACK ASSIGN expression
    | expression
    ;

//
// Function definition as an expression.
functionExp
    : FUNCTION funcBody
    ;

functionDeclaration
    : FUNCTION functionName funcBody
    ;

functionName
    : (ID | PRINT) (DOT (ID | PRINT))* (COLON (ID | PRINT))?
    ;

funcBody
    : LPAREN paramList? RPAREN block END
    ;

paramList
    : param (COMMA param)*
    ;
param
    : ID
    ;

ifStatement
    : IF expression THEN block (ELSEIF expression THEN block)* (ELSE block)? END
    ;

whileStatement
    : WHILE expression DO block END
    ;

repeatStatement
    : REPEAT block UNTIL expression
    ;

forStatement
    : forNumericStatement
    | forGenericStatement
    ;

forNumericStatement
    : FOR ID ASSIGN expression COMMA expression (COMMA expression)? DO block END
    ;

forGenericStatement
    : FOR nameList IN expList DO block END
    ;

nameList
    : ID (COMMA ID)*
    ;

doStatement
    : DO block END
    ;

returnStatement
    : RETURN expList?
    ;

breakStatement
    : BREAK
    ;

localDeclaration
    : LOCAL (functionDeclaration | varDeclaration)
    ;

varDeclaration
    : varList (ASSIGN expList)?
    ;
