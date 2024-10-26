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