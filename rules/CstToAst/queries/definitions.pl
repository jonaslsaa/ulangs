%% START OF definitions.pl QUERY FILE
% Add the missing link_references/3 predicate
link_references(Declarations, References, LinkedDeclarations) :-
    % Group references by name
    group_references(References, GroupedRefs),
    
    % Link each declaration with its references
    maplist(add_references(GroupedRefs), Declarations, LinkedDeclarations).

% Helper predicates for linking references
group_references(References, GroupedRefs) :-
    findall(Name-Refs, (
        member(Ref, References),
        Ref.name = Name,
        findall(RefLoc, (
            member(R, References),
            R.name = Name,
            R.location = RefLoc
        ), Refs)
    ), Pairs),
    list_to_set(Pairs, GroupedRefs).

add_references(GroupedRefs, Declaration, LinkedDecl) :-
    Declaration.name = Name,
    (member(Name-Refs, GroupedRefs) ->
        LinkedDecl = Declaration.put(references, Refs)
    ;
        LinkedDecl = Declaration.put(references, [])
    ).

% Declarations
get_declaration(Node, json{
    name: Name,
    kind: Kind,
    location: json{file: File, line: Line, col: Col, len: Len}
}) :-
    (
        % Function declarations
        node(Node, 'FunctionDefContext'),
        get_child_with_type(Node, 'IDENTIFIER', IdNode),
        Kind = "function"
    ;
        % Parameter declarations
        node(Node, 'ParamContext'),
        get_child_with_type(Node, 'IDENTIFIER', IdNode),
        Kind = "parameter"
    ;
        % Variable declarations
        node(Node, 'AssignmentContext'),
        get_child_with_type(Node, 'IDENTIFIER', IdNode),
        Kind = "variable"
    ),
    value(IdNode, Name),
    node_location(IdNode, File, Line, Col, Len).

% References
get_reference(Node, json{
    name: Name,
    location: json{file: File, line: Line, col: Col, len: Len}
}) :-
    node(Node, 'AtomContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode),
    value(IdNode, Name),
    node_location(IdNode, File, Line, Col, Len),
    % Exclude declarations
    \+ (get_declaration(_, json{name: Name, location: json{file: File, line: Line, col: Col, len: Len}})).


main :-
    get_all_descendants(1, Descendants),
    
    % Collect all declarations
    findall(Decl, (
        member(Node, Descendants),
        get_declaration(Node, Decl)
    ), Declarations),
    
    % Collect all references
    findall(Ref, (
        member(Node, Descendants),
        get_reference(Node, Ref)
    ), References),
    
    % Link references to declarations
    link_references(Declarations, References, LinkedDeclarations),
    
    % Create final JSON structure
    Json = json{symbols: LinkedDeclarations},
    
    % Output as JSON
    json_write_dict(current_output, Json, [width(80)]).

:- main, halt.

%% END OF definitions.pl QUERY FILE
