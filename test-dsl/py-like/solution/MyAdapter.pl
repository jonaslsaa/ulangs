declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionDefContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'ParamContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'AssignmentContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    (
        has_type(Node, 'ExpressionContext')
        ; has_type(Node, 'AtomContext')
        ; has_type(Node, 'FunctionCallContext')
        ; has_type(Node, 'PrintStatementContext')
    ),
    get_child_with_type(Node, 'IDENTIFIER', IdNode),
    \+ declaration_node(_, _, IdNode).

identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).
