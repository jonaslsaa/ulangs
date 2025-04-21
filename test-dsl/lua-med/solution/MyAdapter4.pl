% ------------------------------------------------------------------------------
% Adapter – Lua declarations / references  (fixed helpers & logic)
% ------------------------------------------------------------------------------

% --------------------------------------------------------------------------
% Helpers (needed by declaration/reference logic)
% --------------------------------------------------------------------------
descendant_identifier(Where, Id) :-
    get_all_descendants(Where, Ds),
    member(Id, Ds),
    has_type(Id, 'ID').

earlier_in_file(IdA, IdB) :-
    node_location(IdA, _, LA, CA, _),
    node_location(IdB, _, LB, CB, _),
    (LA < LB ; (LA = LB, CA < CB)).

ancestor(Node, Anc) :-
    has_child(Anc, Node).
ancestor(Node, Anc) :-
    has_child(Parent, Node),
    ancestor(Parent, Anc).

identifier_name(IdNode, Name) :-
    has_type(IdNode, 'ID'),
    value(IdNode, Name).

% --------------------------------------------------------------------------
% declaration_node(+CSTNode, -KindAtom, -IdNode)
% --------------------------------------------------------------------------
% 1) Numeric for‑loop variable.
declaration_node(IdNode, "variable", IdNode) :-
    has_type(IdNode, 'ID'),
    child(Parent, IdNode),
    has_type(Parent, 'ForNumericStatementContext').

% 2) Generic for‑in loop variables.
declaration_node(ForGen, "variable", IdNode) :-
    has_type(ForGen, 'ForGenericStatementContext'),
    descendant_identifier(ForGen, IdNode).

% 3) Function parameters in a FuncBody.
declaration_node(FuncBody, "variable", IdNode) :-
    has_type(FuncBody, 'FuncBodyContext'),
    get_child_with_type(FuncBody, 'ParamListContext', PL),
    descendant_identifier(PL, IdNode).

% 4) Explicit local declarations.
declaration_node(LocalDecl, "variable", IdNode) :-
    has_type(LocalDecl, 'LocalDeclarationContext'),
    get_child_with_type(LocalDecl, 'VarDeclarationContext', VD),
    get_child_with_type(VD, 'VarListContext', VL),
    descendant_identifier(VL, IdNode).

% 5) Single formal parameter.
declaration_node(ParamCtx, "variable", IdNode) :-
    has_type(ParamCtx, 'ParamContext'),
    get_child_with_type(ParamCtx, 'ID', IdNode).

% 6) Function name declaration.
declaration_node(FuncDecl, "function", IdNode) :-
    has_type(FuncDecl, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDecl, 'FunctionNameContext', NameCtx),
    get_child_with_type(NameCtx, 'ID', IdNode).

% 7) Global assignment – first appearance only.
declaration_node(Assign, "variable", IdNode) :-
    has_type(Assign, 'AssignmentContext'),
    get_child_with_type(Assign, 'VarListContext', VL),
    descendant_identifier(VL, IdNode),
    identifier_name(IdNode, Name),
    \+ (
      has_type(Prev, 'AssignmentContext'),
      Prev \= Assign,
      get_child_with_type(Prev, 'VarListContext', PVL),
      descendant_identifier(PVL, PrevId),
      identifier_name(PrevId, Name),
      earlier_in_file(PrevId, IdNode)
    ).

% --------------------------------------------------------------------------
% reference_node(+CSTNode, -IdNode)
% --------------------------------------------------------------------------
reference_node(IdNode, IdNode) :-
    has_type(IdNode, 'ID'),
    value(IdNode, _),
    % not classified as declaration
    \+ declaration_node(_, _, IdNode).

% --------------------------------------------------------------------------
% LSP symbol kinds
% --------------------------------------------------------------------------
symbol_kind("variable", 13).
symbol_kind("function", 12).