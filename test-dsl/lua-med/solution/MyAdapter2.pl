% ------------------------------------------------------------------------------
% Adapter – Lua declarations / references  (improved)
% ------------------------------------------------------------------------------

% --------------------------------------------------------------------------
% Helpers
% --------------------------------------------------------------------------
descendant_identifier(Where, Id) :-
    get_all_descendants(Where, Ds),
    member(Id, Ds),
    has_type(Id, 'ID').

earlier_in_file(IdA, IdB) :-
    node_location(IdA, _, LA, CA, _),
    node_location(IdB, _, LB, CB, _),
    ( LA < LB ; LA = LB, CA < CB ).

ancestor(Node, Ancestor) :-
    has_child(Ancestor, Node).
ancestor(Node, Ancestor) :-
    has_child(Parent, Node),
    ancestor(Parent, Ancestor).

% --------------------------------------------------------------------------
% identifier_name(+IdNode, -Name)
% --------------------------------------------------------------------------
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'ID'),
    value(IdNode, Name).

% --------------------------------------------------------------------------
% declaration_node(+CSTNode, -KindAtom, -IdNode)
% --------------------------------------------------------------------------
% Clause 0: For‑numeric loop variable declaration.
declaration_node(Id, "variable", Id) :-
    has_type(Id, 'ID'),
    child(Parent, Id),
    has_type(Parent, 'ForNumericStatementContext').

% Clause 0a: For‑numeric loop (fallback, grabs each ID once).
declaration_node(ForNum, "variable", IdNode) :-
    has_type(ForNum, 'ForNumericStatementContext'),
    descendant_identifier(ForNum, IdNode).

% Clause 0b: Generic for‑loop variable declaration.
declaration_node(ForGen, "variable", IdNode) :-
    has_type(ForGen, 'ForGenericStatementContext'),
    get_child_with_type(ForGen, 'NameListContext', NL),
    children_of_type(NL, 'ID', Ids),
    member(IdNode, Ids).

% Clause 1: Function parameters (inside a FuncBody).
declaration_node(FuncBody, "variable", IdNode) :-
    has_type(FuncBody, 'FuncBodyContext'),
    get_child_with_type(FuncBody, 'ParamListContext', PL),
    descendant_identifier(PL, IdNode).

% Clause 2: Explicit local declaration (`local x, y = …`).
declaration_node(LocalDeclCtx, "variable", IdNode) :-
    has_type(LocalDeclCtx, 'LocalDeclarationContext'),
    get_child_with_type(LocalDeclCtx, 'VarDeclarationContext', VarDecl),
    get_child_with_type(VarDecl, 'VarListContext', VarList),
    descendant_identifier(VarList, IdNode).

% Clause 3: Formal parameter node itself.
declaration_node(ParamCtx, "variable", IdNode) :-
    has_type(ParamCtx, 'ParamContext'),
    get_child_with_type(ParamCtx, 'ID', IdNode).

% Clause 4: Function name (top‑level or local).
declaration_node(FuncDeclCtx, "function", IdNode) :-
    has_type(FuncDeclCtx, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDeclCtx, 'FunctionNameContext', NameCtx),
    get_child_with_type(NameCtx, 'ID', IdNode).

% Clause 5: Table constructor field ( e.g. { foo = 1 } ).
declaration_node(FieldCtx, "variable", IdNode) :-
    has_type(FieldCtx, 'FieldContext'),
    get_child_with_type(FieldCtx, 'ID', IdNode).

% Clause 6: Plain assignment treated as (first) global/up‑value declaration.
%           Each variable becomes a declaration the *first* time it appears
%           on the left‑hand‑side of an assignment.
declaration_node(AssignCtx, "variable", IdNode) :-
    has_type(AssignCtx, 'AssignmentContext'),
    get_child_with_type(AssignCtx, 'VarListContext', VarList),
    descendant_identifier(VarList, IdNode),
    identifier_name(IdNode, Name),
    % ensure no earlier *assignment* declared the same Name
    \+ (
        has_type(PrevAssign, 'AssignmentContext'),
        PrevAssign \= AssignCtx,
        get_child_with_type(PrevAssign, 'VarListContext', PrevVL),
        descendant_identifier(PrevVL, PrevId),
        identifier_name(PrevId, Name),
        earlier_in_file(PrevId, IdNode)
    ).

% --------------------------------------------------------------------------
% reference_node(+CSTNode, -IdNode)
% --------------------------------------------------------------------------
reference_node(IdNode, IdNode) :-
    has_type(IdNode, 'ID'),
    % exclude IDs that we already classified as declarations
    \+ declaration_node(_, _, IdNode),
    % exclude IDs that are table‑field keys (those are handled separately)
    \+ (ancestor(IdNode, Anc), has_type(Anc, 'FieldContext')).

% --------------------------------------------------------------------------
% Extra symbol kinds (LSP numeric values)
% --------------------------------------------------------------------------
symbol_kind("variable", 13).
symbol_kind("function", 12).