lexer grammar SQLLexer;

/*
  Generic SQL lexer with partial case-insensitivity.
  Adjust or expand for your specific SQL dialect.
*/

// ---------------------- //
//       KEYWORDS         //
// ---------------------- //
CREATE        : [Cc] [Rr] [Ee] [Aa] [Tt] [Ee] ;
TABLE         : [Tt] [Aa] [Bb] [Ll] [Ee] ;
PRIMARY       : [Pp] [Rr] [Ii] [Mm] [Aa] [Rr] [Yy] ;
KEY           : [Kk] [Ee] [Yy] ;
FOREIGN       : [Ff] [Oo] [Rr] [Ee] [Ii] [Gg] [Nn] ;
REFERENCES    : [Rr] [Ee] [Ff] [Ee] [Rr] [Ee] [Nn] [Cc] [Ee] [Ss] ;
AUTO_INCREMENT : [Aa] [Uu] [Tt] [Oo] [_]? [Ii] [Nn] [Cc] [Rr] [Ee] [Mm] [Ee] [Nn] [Tt] ;
NOT           : [Nn] [Oo] [Tt] ;
NULL_         : [Nn] [Uu] [Ll] [Ll] ;
INSERT        : [Ii] [Nn] [Ss] [Ee] [Rr] [Tt] ;
INTO          : [Ii] [Nn] [Tt] [Oo] ;
VALUES        : [Vv] [Aa] [Ll] [Uu] [Ee] [Ss] ;
SELECT        : [Ss] [Ee] [Ll] [Ee] [Cc] [Tt] ;
FROM          : [Ff] [Rr] [Oo] [Mm] ;
JOIN          : [Jj] [Oo] [Ii] [Nn] ;
ON            : [Oo] [Nn] ;
WHERE         : [Ww] [Hh] [Ee] [Rr] [Ee] ;
GROUP         : [Gg] [Rr] [Oo] [Uu] [Pp] ;
BY            : [Bb] [Yy] ;
ORDER         : [Oo] [Rr] [Dd] [Ee] [Rr] ;
UPDATE        : [Uu] [Pp] [Dd] [Aa] [Tt] [Ee] ;
SET           : [Ss] [Ee] [Tt] ;
DELETE        : [Dd] [Ee] [Ll] [Ee] [Tt] [Ee] ;
DATE          : [Dd] [Aa] [Tt] [Ee] ;
INT           : [Ii] [Nn] [Tt] ;
DECIMAL       : [Dd] [Ee] [Cc] [Ii] [Mm] [Aa] [Ll] ;
VARCHAR       : [Vv] [Aa] [Rr] [Cc] [Hh] [Aa] [Rr] ;
AS            : [Aa] [Ss] ;
DISTINCT      : [Dd] [Ii] [Ss] [Tt] [Ii] [Nn] [Cc] [Tt] ;
AND           : [Aa] [Nn] [Dd] ;
OR            : [Oo] [Rr] ;

// ---------------------- //
//       OPERATORS        //
// ---------------------- //
EQ     : '=' ;
COMMA  : ',' ;
SEMI   : ';' ;
LPAREN : '(' ;
RPAREN : ')' ;
STAR   : '*' ;
LT     : '<' ;
GT     : '>' ;
DOT    : '.' ;

// ---------------------- //
//   LITERALS & IDENTIFIERS
// ---------------------- //
INT_LITERAL
 : [0-9]+
 ;

DEC_LITERAL
 : [0-9]+ '.' [0-9]+
 ;

/*
  Allows single-quoted or double-quoted strings,
  capturing escapes within them.
  Adjust for your desired string behavior.
*/
STRING_LITERAL
 : '\'' ( '\\' . | ~('\\' | '\'') )* '\''
 | '"'  ( '\\' . | ~('\\' | '"') )* '"'
 ;

/*
  Basic identifier rule; won't recognize dots.
  We'll handle dotted identifiers in the parser using DOT.
*/
ID
 : [a-zA-Z_] [a-zA-Z0-9_]*
 ;

// ---------------------- //
//       WHITESPACE       //
// ---------------------- //
WS
 : [ \t\r\n]+ -> skip
 ;

// ---------------------- //
//      COMMENTS          //
// ---------------------- //
LINE_COMMENT
 : '--' ~[\r\n]* -> skip
 ;

BLOCK_COMMENT
 : '/*' .*? '*/' -> skip
 ;
