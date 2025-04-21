% WRITEABLE AREA
% Declaration for table name extracted from an Insert Statement.
declaration_node(Node, "object", IdNode) :-
    has_type(Node, 'InsertStmtContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).


% Pre-existing declaration rules (for other grammars) remain:
declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionDefContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).


declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'ParamContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).


% Reference for column names in the IdentifierList (ColumnContext).
reference_node(Node, IdNode) :-
    has_type(Node, 'ColumnContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode),
    + declaration_node(, , IdNode).


% Existing reference rule for expressions, atoms, and function calls.
reference_node(Node, IdNode) :-
    ( has_type(Node, 'ExpressionContext')
    ; has_type(Node, 'AtomContext')
    ; has_type(Node, 'FunctionCallContext')
    ),
    get_child_with_type(Node, 'IDENTIFIER', IdNode),
    + declaration_node(, , IdNode).


% Extract the textual name from an IDENTIFIER node.
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).