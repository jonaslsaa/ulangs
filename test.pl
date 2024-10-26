% Prolog facts for CST tree
:- discontiguous node/2.
:- discontiguous has_child/2.
:- discontiguous has_value/2.

node(1, 'ProgramContext').
node(2, 'StatementContext').
node(27, 'StatementContext').
node(37, 'StatementContext').
node(3, 'FunctionContext').
node(4, 'DEF').
node(5, 'ID').
node(8, 'ID').
node(10, 'ID').
node(21, 'ID').
node(24, 'ID').
node(29, 'ID').
node(39, 'ID').
node(41, 'ID').
node(46, 'ID').
node(6, 'LPAREN').
node(42, 'LPAREN').
node(47, 'LPAREN').
node(7, 'ParamsContext').
node(9, 'COMMA').
node(52, 'COMMA').
node(11, 'RPAREN').
node(56, 'RPAREN').
node(57, 'RPAREN').
node(12, 'COLON').
node(13, 'NEWLINE').
node(25, 'NEWLINE').
node(26, 'NEWLINE').
node(35, 'NEWLINE').
node(36, 'NEWLINE').
node(14, 'FunctionBodyContext').
node(15, 'IndentedStatementContext').
node(16, 'INDENT').
node(17, 'ReturnStatementContext').
node(18, 'RET').
node(19, 'ExpressionContext').
node(31, 'ExpressionContext').
node(44, 'ExpressionContext').
node(49, 'ExpressionContext').
node(53, 'ExpressionContext').
node(20, 'TermContext').
node(23, 'TermContext').
node(50, 'TermContext').
node(54, 'TermContext').
node(22, 'PLUS').
node(28, 'AssignmentContext').
node(30, 'ASSIGN').
node(32, 'ListLiteralContext').
node(33, 'LBRACKET').
node(34, 'RBRACKET').
node(38, 'MethodCallContext').
node(40, 'DOT').
node(43, 'ArgsContext').
node(48, 'ArgsContext').
node(45, 'FunctionCallContext').
node(51, 'NUMBER').
node(55, 'NUMBER').
node(58, 'EOF').

has_value(4, 'def').
has_value(5, 'add').
has_value(6, '(').
has_value(8, 'a').
has_value(9, ',').
has_value(10, 'b').
has_value(11, ')').
has_value(12, ':').
has_value(18, 'ret').
has_value(21, 'a').
has_value(22, '+').
has_value(24, 'b').
has_value(29, 'a').
has_value(30, '=').
has_value(33, '[').
has_value(34, ']').
has_value(39, 'a').
has_value(40, '.').
has_value(41, 'push').
has_value(42, '(').
has_value(46, 'add').
has_value(47, '(').
has_value(51, '2').
has_value(52, ',').
has_value(55, '4').
has_value(56, ')').
has_value(57, ')').
has_value(58, '<EOF>').

has_child(1, 2). % ProgramContext -> StatementContext
has_child(1, 25). % ProgramContext -> NEWLINE
has_child(1, 26). % ProgramContext -> NEWLINE
has_child(1, 27). % ProgramContext -> StatementContext
has_child(1, 35). % ProgramContext -> NEWLINE
has_child(1, 36). % ProgramContext -> NEWLINE
has_child(1, 37). % ProgramContext -> StatementContext
has_child(1, 58). % ProgramContext -> EOF
has_child(2, 3). % StatementContext -> FunctionContext
has_child(3, 4). % FunctionContext -> DEF
has_child(3, 5). % FunctionContext -> ID
has_child(3, 6). % FunctionContext -> LPAREN
has_child(3, 7). % FunctionContext -> ParamsContext
has_child(3, 11). % FunctionContext -> RPAREN
has_child(3, 12). % FunctionContext -> COLON
has_child(3, 13). % FunctionContext -> NEWLINE
has_child(3, 14). % FunctionContext -> FunctionBodyContext
has_child(7, 8). % ParamsContext -> ID
has_child(7, 9). % ParamsContext -> COMMA
has_child(7, 10). % ParamsContext -> ID
has_child(14, 15). % FunctionBodyContext -> IndentedStatementContext
has_child(15, 16). % IndentedStatementContext -> INDENT
has_child(15, 17). % IndentedStatementContext -> ReturnStatementContext
has_child(17, 18). % ReturnStatementContext -> RET
has_child(17, 19). % ReturnStatementContext -> ExpressionContext
has_child(19, 20). % ExpressionContext -> TermContext
has_child(19, 22). % ExpressionContext -> PLUS
has_child(19, 23). % ExpressionContext -> TermContext
has_child(20, 21). % TermContext -> ID
has_child(23, 24). % TermContext -> ID
has_child(27, 28). % StatementContext -> AssignmentContext
has_child(28, 29). % AssignmentContext -> ID
has_child(28, 30). % AssignmentContext -> ASSIGN
has_child(28, 31). % AssignmentContext -> ExpressionContext
has_child(31, 32). % ExpressionContext -> ListLiteralContext
has_child(32, 33). % ListLiteralContext -> LBRACKET
has_child(32, 34). % ListLiteralContext -> RBRACKET
has_child(37, 38). % StatementContext -> MethodCallContext
has_child(38, 39). % MethodCallContext -> ID
has_child(38, 40). % MethodCallContext -> DOT
has_child(38, 41). % MethodCallContext -> ID
has_child(38, 42). % MethodCallContext -> LPAREN
has_child(38, 43). % MethodCallContext -> ArgsContext
has_child(38, 57). % MethodCallContext -> RPAREN
has_child(43, 44). % ArgsContext -> ExpressionContext
has_child(44, 45). % ExpressionContext -> FunctionCallContext
has_child(45, 46). % FunctionCallContext -> ID
has_child(45, 47). % FunctionCallContext -> LPAREN
has_child(45, 48). % FunctionCallContext -> ArgsContext
has_child(45, 56). % FunctionCallContext -> RPAREN
has_child(48, 49). % ArgsContext -> ExpressionContext
has_child(48, 52). % ArgsContext -> COMMA
has_child(48, 53). % ArgsContext -> ExpressionContext
has_child(49, 50). % ExpressionContext -> TermContext
has_child(50, 51). % TermContext -> NUMBER
has_child(53, 54). % ExpressionContext -> TermContext
has_child(54, 55). % TermContext -> NUMBER

% Generic helpers
% Basic node type checking
has_type(Node, Type) :- node(Node, Type).

% Basic tree traversal
child(Parent, Child) :- has_child(Parent, Child).
value(Node, Value) :- has_value(Node, Value).

% Child type checking and retrieval
get_child_with_type(Parent, Type, Child) :-
    child(Parent, Child),
    has_type(Child, Type).

first_child_of_type(Parent, Type, Child) :-
    child(Parent, Child),
    has_type(Child, Type).

% Multiple children retrieval
get_children_of_type(Parent, Type, Children) :-
    findall(Child, get_child_with_type(Parent, Type, Child), Children).

children_of_type(Parent, Type, Children) :-
    findall(Child, first_child_of_type(Parent, Type, Child), Children).

% Value retrieval helpers
get_child_value(Parent, Type, Value) :-
    get_child_with_type(Parent, Type, Child),
    value(Child, Value).

child_value(Parent, Type, Value) :-
    first_child_of_type(Parent, Type, Child),
    value(Child, Value).

% Collecting values
collect_values(Parent, Type, Values) :-
    findall(Value, (
        get_child_with_type(Parent, Type, Node),
        value(Node, Value)
    ), Values).

child_values(Parent, Type, Values) :-
    findall(Value, (
        first_child_of_type(Parent, Type, Child),
        value(Child, Value)
    ), Values).

% Conversion helpers for lists
convert_expression_list([], []).
convert_expression_list([Node|Nodes], [Expr|Exprs]) :-
    convert_expression(Node, Expr),
    convert_expression_list(Nodes, Exprs).

convert_statement_list([], []).
convert_statement_list([Node|Nodes], [Stmt|Stmts]) :-
    convert_statement(Node, Stmt),
    convert_statement_list(Nodes, Stmts).

% Generic node list conversion
convert_nodes([], _, []).
convert_nodes([Node|Nodes], Converter, [Result|Results]) :-
    call(Converter, Node, Result),
    convert_nodes(Nodes, Converter, Results).

% Type-based pattern matching and conversion
has_child_type(Parent, Type) :-
    first_child_of_type(Parent, Type, _).

convert_by_type(Node, [(Type, Converter)|_], Result) :-
    has_child_type(Node, Type),
    first_child_of_type(Node, Type, Child),
    call(Converter, Child, Result),
    !.
convert_by_type(Node, [_|Rest], Result) :-
    convert_by_type(Node, Rest, Result).

% Nth child retrieval
nth_child_of_type(Parent, Type, N, Child) :-
    children_of_type(Parent, Type, Children),
    nth1(N, Children, Child).

% Value handling helpers
node_value(Node, Value) :-
    (value(Node, Value) ; 
     child(Node, Child), 
     value(Child, Value)),
    !.

to_number(Atom, Number) :-
    atom(Atom),
    atom_number(Atom, Number).

% Convenience predicates for common patterns
get_single_child(Parent, Child) :-
    child(Parent, Child),
    \+ (child(Parent, OtherChild), OtherChild \= Child).

has_direct_value(Node) :-
    value(Node, _).

get_all_descendants(Node, Descendants) :-
    findall(Desc, (
        child(Node, Child),
        (Desc = Child ; get_all_descendants(Child, DescList), member(Desc, DescList))
    ), Descendants).

% Type checking helpers
is_terminal(Node) :-
    \+ child(Node, _).

is_nonterminal(Node) :-
    child(Node, _).

% Error handling helpers
safe_convert(Converter, Node, Result) :-
    catch(call(Converter, Node, Result),
          Error,
          (print_message(warning, Error), fail)).

% CST to AST conversion rules
convert_cst_to_ast(Root, program(Statements)) :-
    children_of_type(Root, 'StatementContext', StmtNodes),
    convert_nodes(StmtNodes, convert_statement, Statements).

% Statement conversion
convert_statement(Node, Statement) :-
    convert_by_type(Node, [
        ('FunctionContext', convert_function),
        ('AssignmentContext', convert_assignment),
        ('MethodCallContext', convert_method_call)
    ], Statement).

% Function conversion
convert_function(Node, function(Name, Params, Body)) :-
    child_value(Node, 'ID', Name),
    first_child_of_type(Node, 'ParamsContext', ParamsNode),
    child_values(ParamsNode, 'ID', Params),
    first_child_of_type(Node, 'FunctionBodyContext', BodyNode),
    first_child_of_type(BodyNode, 'IndentedStatementContext', IndentNode),
    first_child_of_type(IndentNode, 'ReturnStatementContext', ReturnNode),
    first_child_of_type(ReturnNode, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, ReturnExpr),
    Body = [return(ReturnExpr)].

% Assignment conversion
convert_assignment(Node, assign(Id, Value)) :-
    get_child_value(Node, 'ID', Id),
    get_child_with_type(Node, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, Value).

% Method call conversion
convert_method_call(Node, method_call(Object, Method, Args)) :-
    get_child_value(Node, 'ID', Object),
    get_child_with_type(Node, 'ID', MethodNode),
    value(MethodNode, Method),
    get_child_with_type(Node, 'ArgsContext', ArgsNode),
    get_children_of_type(ArgsNode, 'ExpressionContext', ArgNodes),
    convert_expression_list(ArgNodes, Args).

% Expression conversion
convert_expression(Node, Expr) :-
    get_child_with_type(Node, 'ListLiteralContext', _) ->
        Expr = list([])
    ; get_child_with_type(Node, 'FunctionCallContext', CallNode) ->
        convert_function_call(CallNode, Expr)
    ; get_child_with_type(Node, 'PLUS', _) ->
        convert_binary_op(Node, Expr)
    ; get_child_with_type(Node, 'TermContext', TermNode) ->
        convert_term(TermNode, Expr).

% Function call conversion
convert_function_call(Node, func_call(Name, Args)) :-
    get_child_value(Node, 'ID', Name),
    get_child_with_type(Node, 'ArgsContext', ArgsNode),
    get_children_of_type(ArgsNode, 'ExpressionContext', ArgNodes),
    convert_expression_list(ArgNodes, Args).

% Binary operation conversion
convert_binary_op(Node, binary_op(Op, Left, Right)) :-
    get_child_with_type(Node, 'PLUS', _),
    Op = '+',
    get_child_with_type(Node, 'TermContext', LeftNode),
    convert_term(LeftNode, Left),
    get_children_of_type(Node, 'TermContext', [_, RightNode]),
    convert_term(RightNode, Right).

% Term conversion
convert_term(Node, Expr) :-
    (get_child_with_type(Node, 'ID', IdNode) ->
        value(IdNode, Value),
        Expr = id(Value)
    ; get_child_with_type(Node, 'NUMBER', NumNode) ->
        value(NumNode, ValueAtom),
        atom_number(ValueAtom, Value),
        Expr = number(Value)
    ).

% Main query
:- convert_cst_to_ast(1, AST),
   write_canonical(AST),
   nl,
   halt.
