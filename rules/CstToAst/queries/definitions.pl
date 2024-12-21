% Required libraries
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Symbol types
symbol_type("function").
symbol_type("parameter").
symbol_type("variable").

% Abstract declaration identification
% conversion.pl implements these predicates
is_declaration(Node, Kind, Name, IdNode) :-
    symbol_type(Kind),
    declaration_node(Node, Kind, IdNode),
    identifier_name(IdNode, Name).

% Abstract reference identification
is_reference(Node, Name, IdNode) :-
    reference_node(Node, IdNode),
    identifier_name(IdNode, Name).

% Symbol collection
get_declaration(Node, Symbol) :-
    is_declaration(Node, Kind, Name, IdNode),
    node_location(IdNode, File, Line, Col, Len),
    Symbol = json{
        name: Name,
        kind: Kind,
        location: json{
            file: File,
            line: Line,
            col: Col,
            len: Len
        }
    }.

% Reference collection
get_reference(Node, Reference) :-
    is_reference(Node, Name, IdNode),
    node_location(IdNode, File, Line, Col, Len),
    Reference = json{
        name: Name,
        location: json{
            file: File,
            line: Line,
            col: Col,
            len: Len
        }
    },
    % Exclude declarations
    \+ (get_declaration(_, json{
        name: Name,
        location: json{
            file: File,
            line: Line,
            col: Col,
            len: Len
        }
    })).

% Reference linking
link_references(Declarations, References, LinkedDeclarations) :-
    group_references(References, GroupedRefs),
    maplist(add_references(GroupedRefs), Declarations, LinkedDeclarations).

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

% Main entry point
main :-
    get_all_nodes(AllNodes),
    
    % Get declarations
    findall(Decl, (
        member(Node, AllNodes),
        get_declaration(Node, Decl)
    ), AllDecls),
    sort(AllDecls, Declarations),
    
    % Get references
    findall(Ref, (
        member(Node, AllNodes),
        get_reference(Node, Ref)
    ), AllRefs),
    sort(AllRefs, References),
    
    % Link references to declarations
    link_references(Declarations, References, LinkedDeclarations),
    
    % Output JSON
    Json = json{symbols: LinkedDeclarations},
    json_write_dict(current_output, Json, [width(80)]).

:- main, halt.
