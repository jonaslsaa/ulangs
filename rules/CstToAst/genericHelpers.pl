% Helper predicates
is_type(Node, Type) :- node(Node, Type).

% Helper predicates for tree traversal
child(Parent, Child) :- has_child(Parent, Child).
value(Node, Value) :- has_value(Node, Value).
get_child_with_type(Parent, Type, Child) :-
    child(Parent, Child),
    is_type(Child, Type).

get_child_value(Parent, Type, Value) :-
    get_child_with_type(Parent, Type, Child),
    value(Child, Value).

get_children_of_type(Parent, Type, Children) :-
    findall(Child, get_child_with_type(Parent, Type, Child), Children).

% Helper for collecting values from nodes of a specific type
collect_values(Parent, Type, Values) :-
    findall(Value, (
        get_child_with_type(Parent, Type, Node),
        value(Node, Value)
    ), Values).

% Helper for expressions
convert_expression_list([], []).
convert_expression_list([Node|Nodes], [Expr|Exprs]) :-
    convert_expression(Node, Expr),
    convert_expression_list(Nodes, Exprs).

% Helper for converting statement lists
convert_statement_list([], []).
convert_statement_list([Node|Nodes], [Stmt|Stmts]) :-
    convert_statement(Node, Stmt),
    convert_statement_list(Nodes, Stmts).