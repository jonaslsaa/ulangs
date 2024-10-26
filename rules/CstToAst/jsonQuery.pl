% Handle lists specially
node_to_json(Node, JSON) :-
    Node =.. [Type|Args],
    clean_args(Args, CleanArgs),
    JSON = json([type=Type, args=CleanArgs]).

% Clean arguments, converting Prolog lists to JSON arrays
clean_args([], []).
clean_args([H|T], [Clean|Rest]) :-
    clean_arg(H, Clean),
    clean_args(T, Rest).

clean_arg(Arg, Clean) :-
    (is_list(Arg) ->
        maplist(node_to_json, Arg, Clean)  % Convert list elements
    ; compound(Arg) ->
        node_to_json(Arg, Clean)    % Convert compound term
    ;
        Clean = Arg    % Keep atomic values as is
    ).

% Helper to check if term is a list
is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|T]) :- is_list(T).


% Convert arguments recursively
args_to_json([], []).
args_to_json([H|T], [JSON|Rest]) :-
    (compound(H) -> 
        node_to_json(H, JSON)    % If compound term, convert recursively
    ; is_list(H) ->
        maplist(node_to_json, H, JSON)  % If list, convert each element
    ;
        JSON = H    % If atomic, use directly
    ),
    args_to_json(T, Rest).

% Main query
:- convert_cst_to_ast(1, AST),
    node_to_json(AST, JSON),
    json_write(current_output, JSON, [width(0)]),
    nl,
    halt.
