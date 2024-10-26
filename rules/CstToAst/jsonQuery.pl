% Generic node to JSON conversion
node_to_json(Node, JSON) :-
    Node =.. [Type|Args],  % Decompose node into type and arguments
    args_to_json(Args, ArgsJSON),
    JSON = json([
        type=Type,
        args=ArgsJSON
    ]).

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
