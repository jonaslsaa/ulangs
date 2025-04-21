% ------------------------------------------------------------------------------
% Adapter – Lua declarations / references
% ------------------------------------------------------------------------------

% --------------------------------------------------------------------------
% helpers
% --------------------------------------------------------------------------
descendant_identifier(Where, Id) :-
    get_all_descendants(Where, Ds),
    member(Id, Ds),
    has_type(Id, 'ID').

earlier_in_file(IdA, IdB) :-
    node_location(IdA, _, LA, CA, _),
    node_location(IdB, _, LB, CB, _),
    ( LA  < LB
    ; LA = LB, CA < CB
    ).

% --------------------------------------------------------------------------
% declaration_node(+CSTNode, -KindAtom, -IdNode)
% --------------------------------------------------------------------------

% 1.  explicit *local* declaration
declaration_node(LocalDeclCtx, "variable", IdNode) :-
    has_type(LocalDeclCtx, 'LocalDeclarationContext'),
    get_child_with_type(LocalDeclCtx, 'VarDeclarationContext', VarDecl),
    get_child_with_type(VarDecl, 'VarListContext', VarList),
    descendant_identifier(VarList, IdNode).

% 2.  formal parameters
declaration_node(ParamCtx, "variable", IdNode) :-
    has_type(ParamCtx, 'ParamContext'),
    get_child_with_type(ParamCtx, 'ID', IdNode).

% 3.  function name
declaration_node(FuncDeclCtx, "function", IdNode) :-
    has_type(FuncDeclCtx, 'FunctionDeclarationContext'),
    get_child_with_type(FuncDeclCtx, 'FunctionNameContext', NameCtx),
    get_child_with_type(NameCtx, 'ID', IdNode).

% 4.  first plain assignment acts as declaration (global / up‑value)
declaration_node(AssignCtx, "variable", IdNode) :-
    has_type(AssignCtx, 'AssignmentContext'),
    get_child_with_type(AssignCtx, 'VarListContext', VarList),
    descendant_identifier(VarList, IdNode),
    identifier_name(IdNode, Name),
    \+ ( has_type(Prev, 'AssignmentContext'),
         get_child_with_type(Prev, 'VarListContext', PV),
         descendant_identifier(PV, PrevId),
         identifier_name(PrevId, Name),
         earlier_in_file(PrevId, IdNode)
       ).

% --------------------------------------------------------------------------
% reference_node(+CSTNode, -IdNode)
% --------------------------------------------------------------------------
reference_node(IdNode, IdNode) :-
    has_type(IdNode, 'ID'),
    \+ declaration_node(_, _, IdNode).

% --------------------------------------------------------------------------
% identifier_name(+IdNode, -Name)
% --------------------------------------------------------------------------
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'ID'),
    value(IdNode, Name).

% --------------------------------------------------------------------------
% extra symbol kinds used here
% --------------------------------------------------------------------------
symbol_kind("variable", 13).
symbol_kind("function", 12).
