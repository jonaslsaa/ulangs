% CST to AST conversion rules
convert_cst_to_ast(Root, program(Statements)) :-
    get_children_of_type(Root, 'StatementContext', StmtNodes),
    convert_statement_list(StmtNodes, Statements).

% Statement conversion
convert_statement(Node, Statement) :-
    (get_child_with_type(Node, 'FunctionContext', FuncNode) ->
        convert_function(FuncNode, Statement)
    ; get_child_with_type(Node, 'AssignmentContext', AssignNode) ->
        convert_assignment(AssignNode, Statement)
    ; get_child_with_type(Node, 'MethodCallContext', MethodNode) ->
        convert_method_call(MethodNode, Statement)
    ).

% Function conversion
convert_function(Node, function(Name, Params, Body)) :-
    get_child_value(Node, 'ID', Name),
    get_child_with_type(Node, 'ParamsContext', ParamsNode),
    collect_values(ParamsNode, 'ID', Params),
    get_child_with_type(Node, 'FunctionBodyContext', BodyNode),
    get_child_with_type(BodyNode, 'IndentedStatementContext', IndentNode),
    get_child_with_type(IndentNode, 'ReturnStatementContext', ReturnNode),
    get_child_with_type(ReturnNode, 'ExpressionContext', ExprNode),
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