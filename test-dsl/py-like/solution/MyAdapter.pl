% Node type checking
node_type(Node, Type) :- node(Node, Type).
node_text(Node, Text) :- has_value(Node, Text).
node_children(Node, Children) :- findall(Child, has_child(Node, Child), Children).

% Built-in identifiers and keywords
is_builtin_identifier(Name) :-
    member(Name, ['def', 'if', 'else', 'ret', 'print', '(', ')', '<', '>', '=']).

% Get identifier name safely
identifier_name(IdNode, Name) :-
    node_type(IdNode, 'IDENTIFIER'),
    node_text(IdNode, Name),
    \+ is_builtin_identifier(Name).

% Declaration node patterns
declaration_node(Node, "function", IdNode) :-
    node_type(Node, 'FunctionDefContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    % Verify it's the function name (not a parameter)
    node_location(IdNode, _, _, 4, _).

declaration_node(Node, "parameter", IdNode) :-
    node_type(Node, 'ParamContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER').

declaration_node(Node, "variable", IdNode) :-
    node_type(Node, 'AssignmentContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    % Ensure it's the first identifier in the assignment
    \+ (member(OtherIdNode, Children),
        node_type(OtherIdNode, 'IDENTIFIER'),
        node_location(OtherIdNode, _, _, OtherCol, _),
        node_location(IdNode, _, _, Col, _),
        OtherCol < Col).

% Reference node patterns
reference_node(Node, IdNode) :-
    node_type(Node, 'AtomContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER').

% Get all nodes for processing
get_all_nodes(Nodes) :-
    findall(Node, node(Node, _), AllNodes),
    % Filter out nodes we don't need to process
    include(is_relevant_node, AllNodes, Nodes).

% Helper to identify relevant nodes
is_relevant_node(Node) :-
    node_type(Node, Type),
    member(Type, ['FunctionDefContext', 'ParamContext', 'AssignmentContext', 'AtomContext']).

% Location information is already provided by node_location/5 facts
