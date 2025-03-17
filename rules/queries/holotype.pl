% === holotype_query.pl ===
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% --------------------------------------------------------------------------
% SymbolKind facts from the LSP (same as definitions.pl):
% --------------------------------------------------------------------------
symbol_kind("object", 19).  % ...plus your others as needed

% --------------------------------------------------------------------------
% Check if a node is a declaration or reference
% (like in both printAST.pl and definitions.pl)
% --------------------------------------------------------------------------
is_declaration(Node, KindAtom, Name, IdNode) :-
    symbol_kind(KindAtom, _KindCode),
    declaration_node(Node, KindAtom, IdNode),
    identifier_name(IdNode, Name).

is_reference(Node, Name, IdNode) :-
    reference_node(Node, IdNode),
    identifier_name(IdNode, Name).

% --------------------------------------------------------------------------
% 1) Build a single AST node
% --------------------------------------------------------------------------
build_ast(Node, Ast) :-
    % If Node is a declaration
    is_declaration(Node, SymbolKindAtom, Name, IdNode),
    node(Node, AntlrType),
    node_location(IdNode, File, Line, Col, Len),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
      nodeName: AntlrType,
      nodeKind: "declaration",
      symbolKind: SymbolKindAtom,  % e.g. "object"
      name: Name,
      location: json{file:File, line:Line, col:Col, len:Len},
      children: ChildrenAsts
    }, 
    !.

build_ast(Node, Ast) :-
    % If Node is a reference
    is_reference(Node, Name, IdNode),
    node(Node, AntlrType),
    node_location(IdNode, File, Line, Col, Len),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
      nodeName: AntlrType,
      nodeKind: "reference",
      name: Name,
      location: json{file:File, line:Line, col:Col, len:Len},
      children: ChildrenAsts
    },
    !.

build_ast(Node, Ast) :-
    % Otherwise, normal AST node (nodeKind = null)
    node(Node, AntlrType),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
      nodeName: AntlrType,
      nodeKind: null,
      children: ChildrenAsts
    }.

% --------------------------------------------------------------------------
% 2) Gather all declarations and references (like definitions.pl)
% --------------------------------------------------------------------------
get_declaration(Node, Symbol) :-
    is_declaration(Node, KindAtom, Name, IdNode),
    symbol_kind(KindAtom, KindCode),
    node_location(IdNode, File, Line, Col, Len),
    Symbol = json{
      kind: KindCode,
      name: Name,
      location: json{file:File, line:Line, col:Col, len:Len}
    }.

get_reference(Node, Reference) :-
    is_reference(Node, Name, IdNode),
    node_location(IdNode, File, Line, Col, Len),
    Reference = json{
      name: Name,
      location: json{file:File, line:Line, col:Col, len:Len}
    },
    \+ is_declaration(_, _, Name, IdNode).  % exclude self-decl

% Link references to matching declarations
link_references(Declarations, References, LinkedDeclarations) :-
    group_references(References, GroupedRefs),
    maplist(add_references(GroupedRefs), Declarations, LinkedDeclarations).

group_references(References, GroupedRefs) :-
    findall(Name-RefLocs, (
      member(Ref, References),
      Ref.name = Name,
      findall(Loc, (
        member(R, References),
        R.name = Name,
        R.location = Loc
      ), RefLocs)
    ), Pairs),
    list_to_set(Pairs, GroupedRefs).

add_references(GroupedRefs, Declaration, LinkedDecl) :-
    Declaration.name = Name,
    ( member(Name-Refs, GroupedRefs)
    -> LinkedDecl = Declaration.put(references, Refs)
    ;  LinkedDecl = Declaration.put(references, [])
    ).

% --------------------------------------------------------------------------
% 3) Main entry: produce BOTH AST + definitions
% --------------------------------------------------------------------------
main :-
    % a) Build the AST
    findall(Root, node(Root, 'ProgramContext'), Roots),
    maplist(build_ast, Roots, RootAsts),

    % b) Gather declarations and references
    findall(Node, node(Node, _Type), AllNodes),
    findall(Decl, (
      member(N, AllNodes),
      get_declaration(N, Decl)
    ), Declarations0),
    sort(Declarations0, Declarations),
    findall(Ref, (
      member(N, AllNodes),
      get_reference(N, Ref)
    ), References0),
    sort(References0, References),
    link_references(Declarations, References, LinkedDecls),

    % c) Print out a single JSON with both AST + definitions
    Result = json{
      ast: RootAsts,
      symbols: LinkedDecls
    },
    json_write_dict(current_output, Result, [width(80)]),
    halt.

:- main.
