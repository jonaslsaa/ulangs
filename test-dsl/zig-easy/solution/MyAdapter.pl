% WRITEABLE AREA
% Updated adapter for converting CST to AST and extracting symbols per the JSON schema

% Declaration node definitions:
declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionDeclContext'),
    get_child_with_type(Node, 'ID', IdNode).

declaration_node(Node, "constant", IdNode) :-
    has_type(Node, 'DeclarationStmtContext'),
    get_child_with_type(Node, 'ID', IdNode).

declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'ParameterContext'),
    get_child_with_type(Node, 'ID', IdNode).

% Reference node definition:
reference_node(Node, IdNode) :-
    ( has_type(Node, 'ExpressionContext')
    ; has_type(Node, 'FunctionCallContext')
    ),
    get_child_with_type(Node, 'ID', IdNode),
    \+ declaration_node(_, _, IdNode).

% Identifier name extraction:
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'ID'),
    value(IdNode, Name).

% Mapping from symbolKind atom to LSP SymbolKind number:
symbol_kind_number("function", 12).
symbol_kind_number("constant", 14).
symbol_kind_number("variable", 13).

% Override main_ast to produce an object with a 'symbols' key, per the expected JSON schema.
:- abolish(main_ast/0).
main_ast :-
    findall(Root, node(Root, 'ProgramContext'), Roots),
    maplist(build_ast, Roots, AstList),
    collect_symbols_list(AstList, Symbols),
    Output = json{ symbols: Symbols },
    json_write_dict(current_output, Output, [width(80)]),
    halt.

% Helper predicates to collect symbol declarations from the AST

collect_symbols_list([], []).
collect_symbols_list([AST|Rest], Symbols) :-
    collect_symbols(AST, Symbols1),
    collect_symbols_list(Rest, SymbolsRest),
    append(Symbols1, SymbolsRest, Symbols).

collect_symbols(AST, Symbols) :-
    ( get_dict(nodeKind, AST, "declaration")
      -> get_dict(symbolKind, AST, SymStr),
         symbol_kind_number(SymStr, KindNum),
         get_dict(location, AST, Loc),
         get_dict(name, AST, Name),
         Symbol = json{ kind: KindNum, location: Loc, name: Name, references: [] },
         List1 = [Symbol]
      ;  List1 = [] ),
    ( get_dict(children, AST, Children) -> true ; Children = [] ),
    collect_symbols_list(Children, ChildSymbols),
    append(List1, ChildSymbols, Symbols).
    
% [/WRITEABLE AREA]