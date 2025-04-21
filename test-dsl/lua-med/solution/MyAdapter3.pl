% ------------------------------------------------------------------------------
% Adapter – Lua declarations/references with unified globals via dot/bracket
% ------------------------------------------------------------------------------
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% --- Helpers ---------------------------------------------------------------
has_type(N,T):-node(N,T).
child(P,C):-has_child(P,C).
value(N,V):-has_value(N,V).

get_child_with_type(P,T,C):-child(P,C),has_type(C,T).

get_all_descendants(N,Ds):-findall(D,(
  child(N,C),
  (D=C; (get_all_descendants(C,Subs),member(D,Subs)))
),Ds).

earlier(A,B):-node_location(A,_,LA,CA,_),node_location(B,_,LB,CB,_),
  (LA<LB;LA=LB,CA<CB).

ancestor(N,A):-child(A,N).
ancestor(N,A):-child(P,N),ancestor(P,A).

% symbol‐node = ID or STRING literal
symbol_node(N):-has_type(N,'ID').
symbol_node(N):-has_type(N,'STRING').

% strip quotes for STRING names
symbol_name(N,Name):-has_type(N,'ID'),value(N,Name).
symbol_name(N,Name):-has_type(N,'STRING'),value(N,Raw),
  atom_length(Raw,L),L>1,Sub is L-2,
  sub_atom(Raw,1,Sub,1,Name).

% --- Declarations ----------------------------------------------------------
% 1) Numeric for‐loop var
declaration_node(Id,"variable",Id):-
  has_type(Id,'ID'),
  child(P,Id),has_type(P,'ForNumericStatementContext').
% 2) Generic for‐in vars
declaration_node(For,"variable",Id):-
  has_type(For,'ForGenericStatementContext'),
  get_child_with_type(For,'NameListContext',NL),
  descendant_identifier(NL,Id).
% 3) Func params
declaration_node(FB,"variable",Id):-
  has_type(FB,'FuncBodyContext'),
  get_child_with_type(FB,'ParamListContext',PL),
  descendant_identifier(PL,Id).
% 4) Local decl
declaration_node(LD,"variable",Id):-
  has_type(LD,'LocalDeclarationContext'),
  get_child_with_type(LD,'VarDeclarationContext',VD),
  get_child_with_type(VD,'VarListContext',VL),
  descendant_identifier(VL,Id).
% 5) Single ParamContext
declaration_node(PC,"variable",Id):-
  has_type(PC,'ParamContext'),
  get_child_with_type(PC,'ID',Id).
% 6) FunctionDeclarationContext
declaration_node(FD,"function",Id):-
  has_type(FD,'FunctionDeclarationContext'),
  get_child_with_type(FD,'FunctionNameContext',NC),
  get_child_with_type(NC,'ID',Id).
% 7) TableField keys
declaration_node(FC,"object",Id):-
  has_type(FC,'FieldContext'),
  get_child_with_type(FC,'ID',Id).
% 8) Global assignment (plain, .foo or ["foo"]): pick last ID/STRING under LHS
declaration_node(AS,"variable",Sym):-
  has_type(AS,'AssignmentContext'),
  get_child_with_type(AS,'VarListContext',VL),
  get_all_descendants(VL,Ds), include(symbol_node,Ds,SDs), SDs\=[],
  last(SDs,Sym),
  symbol_name(Sym,Name),
  \+ ( has_type(Prev,'AssignmentContext'), Prev\=AS,
       get_child_with_type(Prev,'VarListContext',PVL),
       get_all_descendants(PVL,PD), include(symbol_node,PD,PDS), PDS\=[],
       last(PDS,PS), symbol_name(PS,Name),
       earlier(PS,Sym)
     ).

% descendant_identifier=ID only
descendant_identifier(W,Id):-get_all_descendants(W,Ds),member(Id,Ds),has_type(Id,'ID').

% --- References ------------------------------------------------------------
reference_node(S,N):-symbol_node(S),symbol_name(S,N),
  \+ declaration_node(_,_,S).

% --- Symbol kinds ---------------------------------------------------------
symbol_kind("variable",13).
symbol_kind("function",12).
symbol_kind("object",19).

% --- AST building (unchanged) ---------------------------------------------
is_decl(Node,Kind,Name,Id):-declaration_node(Node,Kind,Id),symbol_name(Id,Name).
is_ref(Node,Name,Id):-reference_node(Id,Name),ancestor(Id,Node).

build_ast(N,Ast):-
  is_decl(N,SK,Name,Id),
  node(N,Type),node_location(Id,File,Line,Col,Len),
  findall(C,(child(N,C0),build_ast(C0,C)),Kids),
  Ast=json{nodeName:Type,nodeKind:"declaration",symbolKind:SK,
           name:Name,location:json{file:File,line:Line,col:Col,len:Len},
           children:Kids},!.
build_ast(N,Ast):-
  is_ref(N,Name,Id),
  node(N,Type),node_location(Id,File,Line,Col,Len),
  findall(C,(child(N,C0),build_ast(C0,C)),Kids),
  Ast=json{nodeName:Type,nodeKind:"reference",name:Name,
           location:json{file:File,line:Line,col:Col,len:Len},
           children:Kids},!.
build_ast(N,Ast):-
  node(N,Type),findall(C,(child(N,C0),build_ast(C0,C)),Kids),
  Ast=json{nodeName:Type,nodeKind:null,children:Kids}.

% --- Symbol collection & grouping ----------------------------------------
scope_block(D,B):- 
  (has_type(D,'ForNumericStatementContext');has_type(D,'ForGenericStatementContext')),
  get_child_with_type(D,'BlockContext',B),!.
scope_block(D,B):-has_type(D,'FunctionDeclarationContext'),
  get_child_with_type(D,'FuncBodyContext',FB),
  get_child_with_type(FB,'BlockContext',B),!.
scope_block(_,B):-node(R,'ProgramContext'),get_child_with_type(R,'BlockContext',B).

get_symbol(Decl,S):-
  declaration_node(Decl,KA,Id),symbol_name(Id,Name),
  symbol_kind(KA,KC),node_location(Id,File,Line,Col,Len),
  scope_block(Decl,Blk),
  findall(json{file:FR,line:LR,col:CR,len:LRn},(
    reference_node(Ref,Name),ancestor(Ref,Blk),
    node_location(Ref,FR,LR,CR,LRn)
  ),Refs),
  S=json{kind:KC,name:Name,location:json{file:File,line:Line,col:Col,len:Len},references:Refs}.

% --- Main -----------------------------------------------------------------
main:-
  findall(R,node(R,'ProgramContext'),Rs),
  maplist(build_ast,Rs,ASTs),
  findall(S,get_symbol(D,S),Syms0),sort(Syms0,Raw),
  % merge same-name symbols (earliest loc, union refs)
  findall(Name,member(S,Raw),Ns0),sort(Ns0,Names),
  findall(M,(
    member(Nm,Names),
    include({Nm}/[X]>>X.name=Nm,Raw,G),
    maplist([X,X.references]>>true,G,RefsList),
    append(RefsList,All),sort(All,Refs),
    maplist([X,X.location]>>true,G,Locs),
    maplist([X,X.kind]>>true,G,Ks),
    Locs=[Loc1|_],Ks=[KC|_],
    M=json{kind:KC,name:Nm,location:Loc1,references:Refs}
  ),Symbols),
  json_write_dict(current_output,json{ast:ASTs,symbols:Symbols},[width(80)]),
  halt.
:- main.