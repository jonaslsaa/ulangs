% Core helpers for symbol gathering
get_declaration(Node, json{
    name: Name,
    kind: Type,
    location: json{file: File, line: Line, col: Col, len: Len}
}) :-
    (
        % Function declaration
        (get_child_with_type(Node, 'FunctionContext', FuncNode),
        get_child_with_type(FuncNode, 'ID', IdNode),
        Type = "function")
    ;
        % Parameter declarations
        (get_child_with_type(Node, 'ParamsContext', ParamsNode),
        get_child_with_type(ParamsNode, 'ID', IdNode),
        Type = "parameter")
    ),
    value(IdNode, Name),
    node_location(IdNode, File, Line, Col, Len).

get_reference(Node, json{
    name: Name,
    location: json{file: File, line: Line, col: Col, len: Len}
}) :-
    get_child_with_type(Node, 'ID', IdNode),
    value(IdNode, Name),
    % Exclude declaration locations
    node_location(IdNode, File, Line, Col, Len),
    \+ ((get_declaration(_, Decl),
         Decl.name = Name,
         Decl.location = json{file: File, line: Line, col: Col, len: Len})).

link_references(Declarations, References, LinkedDeclarations) :-
    maplist(add_references(References), Declarations, LinkedDeclarations).

add_references(AllRefs, Decl, LinkedDecl) :-
    findall(Ref, (
        member(Ref, AllRefs),
        Ref.name = Decl.name
    ), DeclRefs),
    LinkedDecl = Decl.put(references, DeclRefs).

main :-
    % Collect all declarations and references
    findall(Decl, (
        get_all_descendants(1, Descendants),
        member(Node, Descendants),
        get_declaration(Node, Decl)
    ), Declarations),
    
    findall(Ref, (
        get_all_descendants(1, Descendants),
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
