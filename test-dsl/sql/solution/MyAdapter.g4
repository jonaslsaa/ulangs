% [WRITABLE]
declaration_node(Node, "class", IdNode) :-
    has_type(Node, 'CreateTableStmtContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

declaration_node(Node, "property", IdNode) :-
    has_type(Node, 'ColumnDefContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionCallContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(ParentNode, IdNode) :-
    has_child(ParentNode, IdNode),
    has_type(IdNode, 'IDENTIFIER'),
    \+ declaration_node(_, _, IdNode).

identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).
% [/WRITABLE]
