% === definitions_query.pl ===
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% SymbolKind mapping (From the official LSP spec)
symbol_kind("file", 1).
symbol_kind("module", 2).
symbol_kind("namespace", 3).
symbol_kind("package", 4).
symbol_kind("class", 5).
symbol_kind("method", 6).
symbol_kind("property", 7).
symbol_kind("field", 8).
symbol_kind("constructor", 9).
symbol_kind("enum", 10).
symbol_kind("interface", 11).
symbol_kind("function", 12).
symbol_kind("variable", 13).
symbol_kind("constant", 14).
symbol_kind("string", 15).
symbol_kind("number", 16).
symbol_kind("boolean", 17).
symbol_kind("array", 18).
symbol_kind("object", 19).
symbol_kind("key", 20).
symbol_kind("null", 21).
symbol_kind("enum_member", 22).
symbol_kind("struct", 23).
symbol_kind("event", 24).
symbol_kind("operator", 25).
symbol_kind("type_parameter", 26).

% is_declaration(Node, KindAtom, Name, IdNode)
% - Ensures KindAtom is recognized as valid SymbolKind
is_declaration(Node, KindAtom, Name, IdNode) :-
    symbol_kind(KindAtom, _KindCode),
    declaration_node(Node, KindAtom, IdNode),
    identifier_name(IdNode, Name).

is_reference(Node, Name, IdNode) :-
    reference_node(Node, IdNode),
    identifier_name(IdNode, Name).

% Convert a declaration to JSON
get_declaration(Node, Symbol) :-
    is_declaration(Node, KindAtom, Name, IdNode),
    node_location(IdNode, File, Line, Col, Len),
    symbol_kind(KindAtom, KindCode),
    Symbol = json{
        name: Name,
        kind: KindCode,
        location: json{
            file: File,
            line: Line,
            col: Col,
            len: Len
        }
    }.

% Convert a reference to JSON
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
    % Exclude declarations that might match same IdNode
    \+ is_declaration(_, _, Name, IdNode).

% Link references to declarations, group them, etc.
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
    ( member(Name-Refs, GroupedRefs) ->
        LinkedDecl = Declaration.put(references, Refs)
    ;
        LinkedDecl = Declaration.put(references, [])
    ).

% Define get_all_nodes/1 to retrieve all node IDs
get_all_nodes(AllNodes) :-
    findall(Node, node(Node, _Type), AllNodes).

% Main entry point
main :-
    get_all_nodes(AllNodes),
    findall(Decl, (
        member(Node, AllNodes),
        get_declaration(Node, Decl)
    ), DeclarationsUnsorted),
    sort(DeclarationsUnsorted, Declarations),

    findall(Ref, (
        member(Node, AllNodes),
        get_reference(Node, Ref)
    ), ReferencesUnsorted),
    sort(ReferencesUnsorted, References),

    link_references(Declarations, References, LinkedDeclarations),
    Json = json{ symbols: LinkedDeclarations },
    json_write_dict(current_output, Json, [width(80)]).

:- main, halt.
