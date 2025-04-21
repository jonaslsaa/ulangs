% WRITEABLE AREA
% -------------------------------------------------------------------
% 1. Source‑order helpers
% -------------------------------------------------------------------
node_pos(Node, Line, Col) :-
    node_location(Node, _File, Line, Col, _).

earlier_in_file(A, B) :-
    node_pos(A, LA, CA),
    node_pos(B, LB, CB),
    (LA < LB ; (LA == LB, CA < CB)).

% -------------------------------------------------------------------
% 2. LSP SymbolKind facts we use
% -------------------------------------------------------------------
symbol_kind("function", 12).   % Function / method
symbol_kind("variable", 13).   % Local / global / parameter / field
symbol_kind("key",       20).  % Table‑constructor key (Lua)

% -------------------------------------------------------------------
% 3. Parent / ancestor helpers
% -------------------------------------------------------------------
parent(Child, Parent) :- child(Parent, Child).

ancestor(Node, Anc) :- parent(Node, Anc).
ancestor(Node, Anc) :- parent(Node, Mid), ancestor(Mid, Anc).

ancestor_of_type(Node, Type) :-
    ancestor(Node, Anc),
    has_type(Anc, Type).

% -------------------------------------------------------------------
% 4. raw_declaration(+DeclNode, -KindAtom, -IdNode)
%    Enumerate EVERY construct that introduces a new named entity.
%    DeclNode is the CST node carrying the declaration (we tag that in the
%    AST); IdNode is the concrete ‘ID’ token that contains the name string.
% -------------------------------------------------------------------
% 4.1  Function names coming from the explicit Lua
%      “function foo() … end” grammar (not present in the sample but kept
%      for completeness).
raw_declaration(FuncDeclCtx, "function", Id) :-
    has_type(FuncDeclCtx, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDeclCtx, 'FunctionNameContext', NameCtx),
    get_child_with_type(NameCtx, 'ID', Id).

% 4.2  Formal parameter of a function
raw_declaration(ParamCtx, "variable", Id) :-
    has_type(ParamCtx, 'ParamContext'),
    get_child_with_type(ParamCtx, 'ID', Id).

% 4.3  Variables introduced by ‘local’ statements
raw_declaration(VarCtx, "variable", Id) :-
    has_type(VarCtx, 'VarContext'),
    ancestor_of_type(VarCtx, 'LocalDeclarationContext'),
    get_child_with_type(VarCtx, 'PrefixExpWithoutCallContext', P1),
    get_child_with_type(P1, 'PrimaryContext', P2),
    get_child_with_type(P2, 'ID', Id).

% 4.4  Top‑level (global) variables: first simple assignment “foo = …”
raw_declaration(VarCtx, "variable", Id) :-
    has_type(VarCtx, 'VarContext'),
    ancestor_of_type(VarCtx, 'AssignmentContext'),
    \+ ancestor_of_type(VarCtx, 'LocalDeclarationContext'),   % not a local
    % only simple identifiers (no varSuffix yet)
    get_child_with_type(VarCtx, 'PrefixExpWithoutCallContext', P1),
    get_child_with_type(P1, 'PrimaryContext', P2),
    get_child_with_type(P2, 'ID', Id).

% 4.5  Table‑field declarations by first “object.field = …” assignment.
%      We capture the ‘field’ identifier that follows a DOT in the VarList
%      part of an AssignmentContext (left‑hand side).
raw_declaration(VarSuffixCtx, "variable", Id) :-
    has_type(VarSuffixCtx, 'VarSuffixContext'),
    ancestor_of_type(VarSuffixCtx, 'VarListContext'),         % on LHS
    get_child_with_type(VarSuffixCtx, 'DOT', _),
    get_child_with_type(VarSuffixCtx, 'ID', Id).

% 4.6  Keys written as bare identifiers inside a table‑constructor literal
raw_declaration(FieldCtx, "key", Id) :-
    has_type(FieldCtx, 'FieldContext'),
    ancestor_of_type(FieldCtx, 'TableConstructorExpContext'),
    get_child_with_type(FieldCtx, 'ID', Id).

% 4.7  Numeric ‘for’ loop control variable   →   for i = 1,10 do …
raw_declaration(IdNode, "variable", IdNode) :-
    has_type(IdNode, 'ID'),
    ancestor_of_type(IdNode, 'ForNumericStatementContext'),
    ancestor(IdNode, ForCtx),
    has_type(ForCtx, 'ForNumericStatementContext'),
    first_child_of_type(ForCtx, 'ID', IdNode).

% 4.8  Generic ‘for’ loop control variables  →   for k,v in … do
raw_declaration(IdNode, "variable", IdNode) :-
    has_type(IdNode, 'ID'),
    ancestor_of_type(IdNode, 'NameListContext'),
    ancestor_of_type(IdNode, 'ForGenericStatementContext').

% -------------------------------------------------------------------
% 5. declaration_node/3
%    Keep only the *first* declaration for each identifier (source order).
% -------------------------------------------------------------------
declaration_node(Node, Kind, Id) :-
    raw_declaration(Node, Kind, Id),
    identifier_name(Id, Name),
    % ensure this is the earliest declaration for that Name
    \+ ( raw_declaration(PrevNode, _K2, PrevId),
         PrevNode \= Node,
         identifier_name(PrevId, Name),
         earlier_in_file(PrevId, Id)
       ).

% -------------------------------------------------------------------
% 6. reference_node(+HolderNode, -IdNode)
%    Any identifier token that is *not* classified as a declaration.
% -------------------------------------------------------------------
reference_node(Holder, Id) :-
    has_type(Id, 'ID'),
    ancestor(Id, Holder),
    \+ declaration_node(_, _, Id).

% -------------------------------------------------------------------
% 7. identifier_name/2
% -------------------------------------------------------------------
identifier_name(Id, Name) :-
    has_type(Id, 'ID'),
    value(Id, Name).
% -------------------------------------------------------------------
% End WRITEABLE AREA