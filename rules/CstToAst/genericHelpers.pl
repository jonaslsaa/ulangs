% Basic node type checking
has_type(Node, Type) :- node(Node, Type).

% Basic tree traversal
child(Parent, Child) :- has_child(Parent, Child).
value(Node, Value) :- has_value(Node, Value).

% Child type checking and retrieval
get_child_with_type(Parent, Type, Child) :-
    child(Parent, Child),
    has_type(Child, Type).

first_child_of_type(Parent, Type, Child) :-
    child(Parent, Child),
    has_type(Child, Type).

% Multiple children retrieval
get_children_of_type(Parent, Type, Children) :-
    findall(Child, get_child_with_type(Parent, Type, Child), Children).

children_of_type(Parent, Type, Children) :-
    findall(Child, first_child_of_type(Parent, Type, Child), Children).

% Value retrieval helpers
get_child_value(Parent, Type, Value) :-
    get_child_with_type(Parent, Type, Child),
    value(Child, Value).

child_value(Parent, Type, Value) :-
    first_child_of_type(Parent, Type, Child),
    value(Child, Value).

% Collecting values
collect_values(Parent, Type, Values) :-
    findall(Value, (
        get_child_with_type(Parent, Type, Node),
        value(Node, Value)
    ), Values).

child_values(Parent, Type, Values) :-
    findall(Value, (
        first_child_of_type(Parent, Type, Child),
        value(Child, Value)
    ), Values).

% Conversion helpers for lists
convert_expression_list([], []).
convert_expression_list([Node|Nodes], [Expr|Exprs]) :-
    convert_expression(Node, Expr),
    convert_expression_list(Nodes, Exprs).

convert_statement_list([], []).
convert_statement_list([Node|Nodes], [Stmt|Stmts]) :-
    convert_statement(Node, Stmt),
    convert_statement_list(Nodes, Stmts).

% Generic node list conversion
convert_nodes([], _, []).
convert_nodes([Node|Nodes], Converter, [Result|Results]) :-
    call(Converter, Node, Result),
    convert_nodes(Nodes, Converter, Results).

% Type-based pattern matching and conversion
has_child_type(Parent, Type) :-
    first_child_of_type(Parent, Type, _).

convert_by_type(Node, [(Type, Converter)|_], Result) :-
    has_child_type(Node, Type),
    first_child_of_type(Node, Type, Child),
    call(Converter, Child, Result),
    !.
convert_by_type(Node, [_|Rest], Result) :-
    convert_by_type(Node, Rest, Result).

% Nth child retrieval
nth_child_of_type(Parent, Type, N, Child) :-
    children_of_type(Parent, Type, Children),
    nth1(N, Children, Child).

% Value handling helpers
node_value(Node, Value) :-
    (value(Node, Value) ; 
     child(Node, Child), 
     value(Child, Value)),
    !.

to_number(Atom, Number) :-
    atom(Atom),
    atom_number(Atom, Number).

% Convenience predicates for common patterns
get_single_child(Parent, Child) :-
    child(Parent, Child),
    \+ (child(Parent, OtherChild), OtherChild \= Child).

has_direct_value(Node) :-
    value(Node, _).

get_all_descendants(Node, Descendants) :-
    findall(Desc, (
        child(Node, Child),
        (Desc = Child ; get_all_descendants(Child, DescList), member(Desc, DescList))
    ), Descendants).

% Type checking helpers
is_terminal(Node) :-
    \+ child(Node, _).

is_nonterminal(Node) :-
    child(Node, _).

% Error handling helpers
safe_convert(Converter, Node, Result) :-
    catch(call(Converter, Node, Result),
          Error,
          (print_message(warning, Error), fail)).