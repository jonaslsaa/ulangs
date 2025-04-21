% WRITEABLE AREA
% -------------------------------------------------------------------
% 1. Source–order helpers
% -------------------------------------------------------------------
node_pos(Node, Line, Col) :-
    node_location(Node, _File, Line, Col, _).

earlier_in_file(A, B) :-
    node_pos(A, LA, CA),
    node_pos(B, LB, CB),
    ( LA < LB ; LA == LB, CA < CB ).

% -------------------------------------------------------------------
% 2. LSP SymbolKind facts we use
% -------------------------------------------------------------------
symbol_kind("function", 12).   % Function / method
symbol_kind("variable", 13).   % Local / global / parameter
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
%    Enumerates EVERY syntactic construct that introduces a new name.
%    DeclNode is the CST node we tag as “declaration” in the AST,
%    IdNode   is the concrete ‘ID’ token that carries the name text.
% -------------------------------------------------------------------
% 4.1  Function name
raw_declaration(FuncDeclCtx, "function", Id) :-
    has_type(FuncDeclCtx, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDeclCtx, 'FunctionNameContext', NameCtx),
    get_child_with_type(NameCtx, 'ID', Id).

% 4.2  Formal parameter
raw_declaration(ParamCtx, "variable", Id) :-
    has_type(ParamCtx, 'ParamContext'),
    get_child_with_type(ParamCtx, 'ID', Id).

% 4.3  Local variable (`local foo = ...`)
raw_declaration(VarCtx, "variable", Id) :-
    has_type(VarCtx, 'VarContext'),
    ancestor_of_type(VarCtx, 'LocalDeclarationContext'),
    get_child_with_type(VarCtx, 'PrefixExpWithoutCallContext', P1),
    get_child_with_type(P1,      'PrimaryContext',            P2),
    get_child_with_type(P2,      'ID',                        Id).

% 4.4  First assignment to a global variable (simple form `foo = ...`)
raw_declaration(VarCtx, "variable", Id) :-
    has_type(VarCtx, 'VarContext'),
    ancestor_of_type(VarCtx, 'AssignmentContext'),
    \+ ancestor_of_type(VarCtx, 'LocalDeclarationContext'),  % not a local
    get_child_with_type(VarCtx, 'PrefixExpWithoutCallContext', P1),
    get_child_with_type(P1,      'PrimaryContext',            P2),
    get_child_with_type(P2,      'ID',                        Id).

% 4.5  Key inside a table constructor written with bare identifier
raw_declaration(FieldCtx, "key", Id) :-
    has_type(FieldCtx, 'FieldContext'),
    ancestor_of_type(FieldCtx, 'TableConstructorExpContext'),
    get_child_with_type(FieldCtx, 'ID', Id).

% 4.6  Numeric `for` loop control variable  →  for i = 1, 10 do ...
raw_declaration(Id, "variable", Id) :-
    has_type(Id, 'ID'),
    ancestor_of_type(Id, 'ForNumericStatementContext'),
    % Ensure the ID is the control variable (appears before the ASSIGN '=')
    % inside the 'ForNumericStatementContext'
    ancestor(Id, ForCtx),
    has_type(ForCtx, 'ForNumericStatementContext'),
    % the very first ID child of the ForNumericStatementContext is the control var
    first_child_of_type(ForCtx, 'ID', Id).

% 4.7  Generic `for` loop control variables  →  for i, v in ... do
raw_declaration(Id, "variable", Id) :-
    has_type(Id, 'ID'),
    ancestor_of_type(Id, 'NameListContext'),
    ancestor_of_type(Id, 'ForGenericStatementContext').

% -------------------------------------------------------------------
% 5. declaration_node(+DeclNode, -KindAtom, -IdNode)
%    Keep ONLY the very first declaration for a given name in
%    source‑file order.
% -------------------------------------------------------------------
declaration_node(Node, Kind, Id) :-
    raw_declaration(Node, Kind, Id),
    identifier_name(Id, Name),
    % Make sure there is no *earlier* declaration of the same name
    \+ ( raw_declaration(PrevNode, _Kind2, PrevId),
         PrevNode \= Node,
         identifier_name(PrevId, Name),
         earlier_in_file(PrevId, Id)
       ).

% -------------------------------------------------------------------
% 6. reference_node(+SomeNode, -IdNode)
%    Any identifier token that is NOT a declaration.
% -------------------------------------------------------------------
reference_node(Holder, Id) :-
    has_type(Id, 'ID'),
    ancestor(Id, Holder),
    \+ declaration_node(_, _, Id).

% -------------------------------------------------------------------
% 7. identifier_name(+IdNode, -NameAtom)
% -------------------------------------------------------------------
identifier_name(Id, Name) :-
    has_type(Id, 'ID'),
    value(Id, Name).
% -------------------------------------------------------------------
% End WRITEABLE AREA