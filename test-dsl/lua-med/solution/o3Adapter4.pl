% WRITEABLE AREA
% -------------------------------------------------------------------
% 1. Source‑order helpers
% -------------------------------------------------------------------
node_pos(Node, Line, Col) :- node_location(Node, _F, Line, Col, _).
earlier_in_file(A, B)     :- node_pos(A,L1,C1), node_pos(B,L2,C2), (L1<L2 ; L1==L2, C1<C2).

% -------------------------------------------------------------------
% 2. LSP SymbolKind mapping
% -------------------------------------------------------------------
symbol_kind("function", 12).
symbol_kind("variable", 13).
symbol_kind("key",      20).

% -------------------------------------------------------------------
% 3. Ancestor helpers
% -------------------------------------------------------------------
parent(Child, Parent) :- child(Parent, Child).
ancestor(Node, Anc)   :- parent(Node, Anc).
ancestor(Node, Anc)   :- parent(Node, Mid), ancestor(Mid, Anc).
ancestor_of_type(N,T) :- ancestor(N,A), has_type(A,T).

% -------------------------------------------------------------------
% 4. Utilities for VarContext / dotted names
% -------------------------------------------------------------------
varctx_root_id(VarCtx, RootId) :-
    get_child_with_type(VarCtx,'PrefixExpWithoutCallContext',P1),
    get_child_with_type(P1,'PrimaryContext',P2),
    get_child_with_type(P2,'ID',RootId).

property_id(Id, VarCtx) :-
    has_type(Id,'ID'),
    ancestor(Id,Suffix), has_type(Suffix,'VarSuffixContext'),
    child(Suffix,Dot),   has_type(Dot,'DOT'),
    ancestor(Suffix,VarCtx), has_type(VarCtx,'VarContext').

varcontext_last_id(VarCtx, LastId) :-
    findall(Id-Line-Col,(descendant(VarCtx,Id),has_type(Id,'ID'),node_pos(Id,Line,Col)),L),
    sort(L,S), last(S,LastId-_-_).

build_full_path(VarCtx, Id, Path) :-
    varctx_root_id(VarCtx, Root), value(Root,RootName),
    ( property_id(Id,VarCtx)
      -> value(Id,Leaf), atomic_list_concat([RootName,Leaf],'.',Path)
      ;  Path=RootName ).

% generic descendant
descendant(A,B):-child(A,B).
descendant(A,B):-child(A,C),descendant(C,B).

% -------------------------------------------------------------------
% 5. raw_declaration(+Node,-Kind,-Id)
% -------------------------------------------------------------------
raw_declaration(F,"function",Id):-has_type(F,'FunctionDeclarationContext'),
    get_child_with_type(F,'FunctionNameContext',N),get_child_with_type(N,'ID',Id).
raw_declaration(P,"variable",Id):-has_type(P,'ParamContext'),
    get_child_with_type(P,'ID',Id).
raw_declaration(V,"variable",Id):-has_type(V,'VarContext'),
    ancestor_of_type(V,'LocalDeclarationContext'), varcontext_last_id(V,Id).
raw_declaration(V,"variable",Id):-has_type(V,'VarContext'),
    ancestor_of_type(V,'AssignmentContext'),
    \+ ancestor_of_type(V,'LocalDeclarationContext'),
    varcontext_last_id(V,Id).
raw_declaration(F,"key",Id):-has_type(F,'FieldContext'),
    ancestor_of_type(F,'TableConstructorExpContext'),
    get_child_with_type(F,'ID',Id).
raw_declaration(Id,"variable",Id):-has_type(Id,'ID'),
    ancestor_of_type(Id,'ForNumericStatementContext'),
    ancestor(Id,For),has_type(For,'ForNumericStatementContext'),
    first_child_of_type(For,'ID',Id).
raw_declaration(Id,"variable",Id):-has_type(Id,'ID'),
    ancestor_of_type(Id,'NameListContext'),
    ancestor_of_type(Id,'ForGenericStatementContext').

% -------------------------------------------------------------------
% 6. declaration_node
% -------------------------------------------------------------------
declaration_node(Node,K,Id):-raw_declaration(Node,K,Id),
    identifier_name(Id,N),
    \+ (raw_declaration(P,_,Pid), P\=Node, identifier_name(Pid,N), earlier_in_file(Pid,Id)).

% -------------------------------------------------------------------
% 7. reference_node
% -------------------------------------------------------------------
reference_node(Id,Id):-has_type(Id,'ID'), \+ declaration_node(_,_,Id).

% -------------------------------------------------------------------
% 8. identifier_name(+Id,-Name)
%      – handles plain ids and dotted properties everywhere
% -------------------------------------------------------------------
identifier_name(Id,Name):-has_type(Id,'ID'),
    (   % inside a VarContext
        ancestor(Id,VC), has_type(VC,'VarContext')
    ->  ( property_id(Id,VC) -> build_full_path(VC,Id,Name)
         ; value(Id,Name) )
    ;   % identifier after a DOT (but not in VarContext), build Root.Leaf
        ancestor(Id,Suffix), has_type(Suffix,'VarSuffixContext'),
        child(Suffix,Dot),has_type(Dot,'DOT'),
        ancestor(Suffix,PWC), has_type(PWC,'PrefixExpWithoutCallContext'),
        get_child_with_type(PWC,'PrimaryContext',RPC),
        get_child_with_type(RPC,'ID',RootId),
        value(RootId,RootName), value(Id,Leaf),
        atomic_list_concat([RootName,Leaf],'.',Name)
    ;   % fallback plain identifier
        value(Id,Name)
    ).

% -------------------------------------------------------------------
% End WRITEABLE AREA