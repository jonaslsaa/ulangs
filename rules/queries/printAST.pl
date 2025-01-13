:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).


is_declaration(Node, KindAtom, Name, IdNode) :-
    declaration_node(Node, KindAtom, IdNode),
    identifier_name(IdNode, Name).

% is_reference(Node, Name, IdNode):
%   Checks if Node is recognized by the adapter as a reference, then
%   retrieves the identifier Name.
is_reference(Node, Name, IdNode) :-
    reference_node(Node, IdNode),
    identifier_name(IdNode, Name).

% --------------------------------------------------------------------------
% build_ast/2
%   Recursively constructs a JSON-like representation of Node:
%     - nodeName: always the raw ANTLR type (e.g., "ProgramContext")
%     - nodeKind: "declaration", "reference", or null
%     - symbolKind: e.g., "function" or "variable" (only for declarations)
%     - name: the identifier (only for references and declarations)
%     - location: the node's File/Line/Col/Len (only for references/declarations)
%     - children: recursively built AST children
% --------------------------------------------------------------------------

build_ast(Node, Ast) :-
    % 1) If Node is a declaration:
    is_declaration(Node, SymbolKind, Name, IdNode),
    node(Node, AntlrType),
    node_location(IdNode, File, Line, Col, Len),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
        nodeName: AntlrType,
        nodeKind: "declaration",
        symbolKind: SymbolKind,   % e.g. "function", "variable"
        name: Name,
        location: json{file: File, line: Line, col: Col, len: Len},
        children: ChildrenAsts
    },
    !.  % Cut to prevent falling into later clauses.

build_ast(Node, Ast) :-
    % 2) If Node is a reference:
    is_reference(Node, Name, IdNode),
    node(Node, AntlrType),
    node_location(IdNode, File, Line, Col, Len),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
        nodeName: AntlrType,
        nodeKind: "reference",
        name: Name,
        location: json{file: File, line: Line, col: Col, len: Len},
        children: ChildrenAsts
    },
    !.

build_ast(Node, Ast) :-
    % 3) Fallback: no known declaration or reference → nodeKind: null
    node(Node, AntlrType),
    findall(ChildAst, (child(Node, Child), build_ast(Child, ChildAst)), ChildrenAsts),
    Ast = json{
        nodeName: AntlrType,
        nodeKind: null,
        children: ChildrenAsts
    }.

% --------------------------------------------------------------------------
% main_ast/0
%   Finds top-level root nodes (often "ProgramContext"), builds the AST for
%   each, prints them as JSON (list form if multiple roots), then halts.
% --------------------------------------------------------------------------

main_ast :-
    % Typically there's one "ProgramContext", but in case of multiple roots:
    findall(Root, node(Root, 'ProgramContext'), Roots),
    maplist(build_ast, Roots, RootAsts),
    json_write_dict(current_output, RootAsts, [width(80)]),
    halt.

:- main_ast.
