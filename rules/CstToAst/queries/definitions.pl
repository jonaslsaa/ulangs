%% START OF definitions.pl QUERY FILE
% Required libraries
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Abstract interface predicates - to be implemented by conversion.pl
% node_type(Node, Type) :- node(Node, Type).
% node_text(Node, Text) :- has_value(Node, Text).
% node_children(Node, Children) :- findall(Child, has_child(Node, Child), Children).

% Symbol types
symbol_type("function").
symbol_type("parameter").
symbol_type("variable").

% Built-in identifiers to exclude
is_builtin(Name) :-
    member(Name, ['def', 'if', 'else', 'ret', 'print']).

% Declaration patterns
is_declaration(Node, "function", Name, IdNode) :-
    node_type(Node, 'FunctionDefContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    node_text(IdNode, Name),
    \+ is_builtin(Name),
    % Ensure we only get the function name identifier
    node_location(IdNode, _, _, Col, _),
    Col = 4.  % Specific column for function name

is_declaration(Node, "parameter", Name, IdNode) :-
    node_type(Node, 'ParamContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    node_text(IdNode, Name),
    \+ is_builtin(Name),
    % Ensure we get only the parameter declaration
    node_location(IdNode, _, _, Col, _),
    Col = 14.  % Specific column for parameter

is_declaration(Node, "variable", Name, IdNode) :-
    node_type(Node, 'AssignmentContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    node_text(IdNode, Name),
    \+ is_builtin(Name),
    % First identifier in assignment
    \+ (member(OtherIdNode, Children),
        node_type(OtherIdNode, 'IDENTIFIER'),
        node_location(OtherIdNode, _, _, OtherCol, _),
        node_location(IdNode, _, _, Col, _),
        OtherCol < Col).

% Symbol collection
get_declaration(Node, Symbol) :-
    symbol_type(Kind),
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
    node_type(Node, 'AtomContext'),
    node_children(Node, Children),
    member(IdNode, Children),
    node_type(IdNode, 'IDENTIFIER'),
    node_text(IdNode, Name),
    \+ is_builtin(Name),
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

% Deduplication helper
get_declarations(Nodes, Declarations) :-
    findall(Decl, (
        member(Node, Nodes),
        get_declaration(Node, Decl)
    ), AllDecls),
    sort(AllDecls, Declarations).

% Main entry point
main :-
    findall(Node, node_type(Node, _), AllNodes),
    
    % Get unique declarations
    get_declarations(AllNodes, Declarations),
    
    % Get references
    findall(Ref, (
        member(Node, AllNodes),
        get_reference(Node, Ref)
    ), AllRefs),
    sort(AllRefs, References),  % Remove duplicate references
    
    % Link references to declarations
    link_references(Declarations, References, LinkedDeclarations),
    
    % Output JSON
    Json = json{symbols: LinkedDeclarations},
    json_write_dict(current_output, Json, [width(80)]).

:- main, halt.

%% END OF definitions.pl QUERY FILE
