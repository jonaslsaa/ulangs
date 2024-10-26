% Main node to JSON conversion with location support
node_to_json(ast_node(Node, Location), JSON) :- !,
    Node =.. [Type|Args],
    clean_args(Args, CleanArgs),
    location_to_json(Location, LocJSON),
    JSON = json([
        type=Type,
        args=CleanArgs,
        location=LocJSON
    ]).

% Handle regular nodes without location
node_to_json(Node, JSON) :-
    Node =.. [Type|Args],
    clean_args(Args, CleanArgs),
    JSON = json([
        type=Type,
        args=CleanArgs
    ]).

% Convert location information to JSON
location_to_json(location(File, Line, Col, Len), JSON) :- !,
    JSON = json([
        file=File,
        line=Line,
        column=Col,
        length=Len
    ]).

% Handle missing location
location_to_json(_, null).

% Clean arguments with improved list handling
clean_args([], []).
clean_args([H|T], [Clean|Rest]) :-
    clean_arg(H, Clean),
    clean_args(T, Rest).

% Enhanced clean_arg with better type handling
clean_arg(Arg, Clean) :-
    (is_list(Arg) ->
        maplist(node_to_json, Arg, Clean)  % Convert list elements
    ; ast_node(_, _) = Arg ->
        node_to_json(Arg, Clean)    % Handle ast_node specifically
    ; compound(Arg) ->
        node_to_json(Arg, Clean)    % Convert other compound terms
    ; number(Arg) ->
        Clean = Arg    % Keep numbers as is
    ; atom(Arg) ->
        Clean = Arg    % Keep atoms as is
    ; var(Arg) ->
        Clean = null   % Handle variables
    ;
        Clean = Arg    % Default case
    ).

% Helper predicates remain the same
is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|T]) :- is_list(T).

args_to_json([], []).
args_to_json([H|T], [JSON|Rest]) :-
    (compound(H) -> 
        node_to_json(H, JSON)
    ; is_list(H) ->
        maplist(node_to_json, H, JSON)
    ;
        JSON = H
    ),
    args_to_json(T, Rest).

% Main query with pretty printing
:- convert_cst_to_ast(1, AST),
    node_to_json(AST, JSON),
    json_write(current_output, JSON, [width(0)]),
    nl,
    halt.
