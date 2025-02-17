% WRITEABLE AREA
% Declaration: Function declarations are identified by 'FunctionDeclContext'
declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionDeclContext'),
    get_child_with_type(Node, 'ID', IdNode).

% Declaration: Constant declarations from 'DeclarationStmtContext' (e.g. const definitions)
declaration_node(Node, "constant", IdNode) :-
    has_type(Node, 'DeclarationStmtContext'),
    get_child_with_type(Node, 'ID', IdNode).

% Reference: Expressions, primary expressions or function calls referring to an identifier.
reference_node(Node, IdNode) :-
    ( has_type(Node, 'ExpressionContext')
    ; has_type(Node, 'PrimaryContext')
    ; has_type(Node, 'FunctionCallContext')
    ),
    get_child_with_type(Node, 'ID', IdNode),
    \+ ( declaration_node(_, _, IdNode) ).

% Identifier name extraction: Ensures the returned name is a string.
identifier_name(IdNode, Name) :-
    ( has_type(IdNode, 'ID')
    ; has_type(IdNode, 'IDENTIFIER')
    ),
    value(IdNode, Val),
    ( Val == false -> Name = "false"
    ; atom(Val) -> Name = Val
    ; term_string(Val, Name)
    ).
% End of WRITEABLE AREA