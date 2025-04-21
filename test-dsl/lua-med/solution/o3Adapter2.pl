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
symbol_kind("function", 12).
symbol_kind("variable", 13).
symbol_kind("key",       20).

% -------------------------------------------------------------------
% 3. Ancestor helpers
% -------------------------------------------------------------------
parent(Child, Parent) :- child(Parent, Child).

ancestor(Node, Anc) :- parent(Node, Anc).
ancestor(Node, Anc) :- parent(Node, Mid), ancestor(Mid, Anc).

ancestor_of_type(Node, Type) :-
    ancestor(Node, Anc),
    has_type(Anc, Type).

% -------------------------------------------------------------------
% 4. raw_declaration(+CSTNode, -KindAtom, -IdNode)
%    Enumerates EVERY context that introduces an identifier.
% -------------------------------------------------------------------
% 4.1  Function name
raw_declaration(FuncDecl, "function", Id) :-
    has_type(FuncDecl, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDecl, 'FunctionNameContext', NameCtx),
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

% 4.4  First assignment to a global variable
raw_declaration(VarCtx, "variable", Id) :-
    has_type(VarCtx, 'VarContext'),
    ancestor_of_type(VarCtx, 'AssignmentContext'),
    \+ ancestor_of_type(VarCtx, 'LocalDeclarationContext'),
    get_child_with_type(VarCtx, 'PrefixExpWithoutCallContext', P1),
    get_child_with_type(P1,      'PrimaryContext',            P2),
    get_child_with_type(P2,      'ID',                        Id).

% 4.5  Table‑constructor key (identifier form)
raw_declaration(FieldCtx, "key", Id) :-
    has_type(FieldCtx, 'FieldContext'),
    ancestor_of_type(FieldCtx, 'TableConstructorExpContext'),
    get_child_with_type(FieldCtx, 'ID', Id).

% 4.6  Numeric `for` loop control variable  (NEW)
raw_declaration(ForCtx, "variable", Id) :-
    has_type(ForCtx, 'ForNumericStatementContext'),
    get_child_with_type(ForCtx, 'ID', Id).

% -------------------------------------------------------------------
% 5. declaration_node(+Node, -KindAtom, -IdNode)
%    Keep ONLY the *earliest* declaration for each identifier.
% -------------------------------------------------------------------
declaration_node(Node, Kind, Id) :-
    raw_declaration(Node, Kind, Id),
    identifier_name(Id, Name),
    \+ ( raw_declaration(PrevNode, _Kind2, PrevId),
         PrevNode \= Node,
         identifier_name(PrevId, Name),
         earlier_in_file(PrevId, Id)
       ).

% -------------------------------------------------------------------
% 6. reference_node(+IdNodeHolder, -IdNode)
%    Any identifier token that is not part of its own declaration.
% -------------------------------------------------------------------
reference_node(Id, Id) :-
    has_type(Id, 'ID'),
    \+ declaration_node(_, _, Id).

% -------------------------------------------------------------------
% 7. identifier_name(+IdNode, -NameAtom)
% -------------------------------------------------------------------
identifier_name(Id, Name) :-
    has_type(Id, 'ID'),
    value(Id, Name).
% -------------------------------------------------------------------
% End WRITEABLE AREA