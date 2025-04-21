% WRITEABLE AREA
% ------------------------------------------------------------------------------
% Adapter to correctly extract user-defined symbols in SQL queries.
% Built-in SQL keywords (SELECT, FROM, WHERE) are ignored in the symbol list.
% ------------------------------------------------------------------------------
%
% Symbol kinds:
% - "variable" (13) for table names.
% - "field" (8) for column names.
% - "string" (15) for string literals.

% ------------------------------------------------------------------------------
% 1) declaration_node(+ASTNode, -KindAtom, -IdNode)
% ------------------------------------------------------------------------------
% Capture table names in FROM clause as "variable".
declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'FromClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture column names in SELECT clause as "field".
declaration_node(Node, "field", IdNode) :-
    has_type(Node, 'IdentifierListContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture column names in WHERE clause as "field".
declaration_node(Node, "field", IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture string literals in WHERE clause as "string".
declaration_node(Node, "string", IdNode) :-
    has_type(Node, 'ValueContext'),
    get_child_with_type(Node, 'STRING_LITERAL', IdNode).

% ------------------------------------------------------------------------------
% 2) reference_node(+ASTNode, -IdNode)
% ------------------------------------------------------------------------------
% References are similar to declarations, since SQL is mostly usage-based.

reference_node(Node, IdNode) :-
    has_type(Node, 'SelectStmtContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'FromClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'STRING_LITERAL', IdNode).

% ------------------------------------------------------------------------------
% 3) identifier_name(+IdNode, -Name)
% ------------------------------------------------------------------------------
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).

identifier_name(IdNode, Name) :-
    has_type(IdNode, 'STRING_LITERAL'),
    value(IdNode, Name).

% End of adapter
% ------------------------------------------------------------------------------
% [/WRITABLE]