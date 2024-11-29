% CST to AST conversion rules
%% START OF CONVERSION.pl FILE

% Start converting the CST to AST from the root node
convert_cst_to_ast(Root, program(Functions)) :-
    children_of_type(Root, 'FunctionDefContext', FunctionNodes),
    convert_nodes(FunctionNodes, convert_function_def, Functions).

% Function definition conversion
convert_function_def(Node, function(Name, Params, ReturnType, Body)) :-
    get_child_value(Node, 'IDENTIFIER', Name),
    get_child_with_type(Node, 'ParamListContext', ParamsNode),
    convert_param_list(ParamsNode, Params),
    get_child_with_type(Node, 'TypeAnnotationContext', ReturnTypeNode),
    convert_type_annotation(ReturnTypeNode, ReturnType),
    get_child_with_type(Node, 'BlockContext', BodyNode),
    convert_block(BodyNode, Body).

% Parameter list conversion
convert_param_list(Node, Params) :-
    get_children_of_type(Node, 'ParamContext', ParamNodes),
    convert_nodes(ParamNodes, convert_param, Params).

% Parameter conversion
convert_param(Node, param(Name, Type)) :-
    get_child_value(Node, 'IDENTIFIER', Name),
    get_child_with_type(Node, 'TypeAnnotationContext', TypeAnnNode),
    convert_type_annotation(TypeAnnNode, Type).

% Type annotation conversion
convert_type_annotation(Node, Type) :-
    get_child_with_type(Node, 'TypeContext', TypeNode),
    get_child_value(TypeNode, 'INT_TYPE', Type).

% Block conversion
convert_block(Node, Statements) :-
    children_of_type(Node, 'StatementContext', StmtNodes),
    convert_nodes(StmtNodes, convert_statement, Statements).

% Statement conversion
convert_statement(Node, Statement) :-
    (get_child_with_type(Node, 'IfStatementContext', IfNode) ->
        convert_if_statement(IfNode, Statement)
    ; get_child_with_type(Node, 'ReturnStatementContext', ReturnNode) ->
        convert_return_statement(ReturnNode, Statement)
    ; get_child_with_type(Node, 'AssignmentContext', AssignNode) ->
        convert_assignment(AssignNode, Statement)
    ; get_child_with_type(Node, 'PrintStatementContext', PrintNode) ->
        convert_print_statement(PrintNode, Statement)
    ).

% If statement conversion
convert_if_statement(Node, if(Condition, ThenBody, ElseBody)) :-
    get_child_with_type(Node, 'ExpressionContext', CondNode),
    convert_expression(CondNode, Condition),
    get_children_of_type(Node, 'BlockContext', BlockNodes),
    nth1(1, BlockNodes, ThenBlockNode),
    convert_block(ThenBlockNode, ThenBody),
    (nth1(2, BlockNodes, ElseBlockNode) ->
        convert_block(ElseBlockNode, ElseBody)
    ; ElseBody = []
    ).

% Return statement conversion
convert_return_statement(Node, return(Expression)) :-
    get_child_with_type(Node, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, Expression).

% Assignment conversion
convert_assignment(Node, assign(Name, Type, Expression)) :-
    get_child_value(Node, 'IDENTIFIER', Name),
    (get_child_with_type(Node, 'TypeAnnotationContext', TypeAnnNode) ->
        convert_type_annotation(TypeAnnNode, Type)
    ; Type = none),
    get_child_with_type(Node, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, Expression).

% Print statement conversion
convert_print_statement(Node, print(Expression)) :-
    get_child_with_type(Node, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, Expression).

% Expression conversion
convert_expression(Node, Expression) :-
    (get_child_with_type(Node, 'AtomContext', AtomNode) ->
        convert_atom(AtomNode, Expression)
    ; get_children_of_type(Node, 'ExpressionContext', ExprNodes),
      length(ExprNodes, 2),
      ExprNodes = [LeftNode, RightNode],
      % Find operator node of any type that's not an expression
      child(Node, OpNode),
      \+ has_type(OpNode, 'ExpressionContext'),
      value(OpNode, Op),
      convert_expression(LeftNode, LeftExpr),
      convert_expression(RightNode, RightExpr),
      Expression = binary_op(Op, LeftExpr, RightExpr)
    ; get_child_with_type(Node, 'FunctionCallContext', FuncCallNode) ->
        convert_function_call(FuncCallNode, Expression)
    ).
    
% Atom conversion
convert_atom(Node, id(Name)) :-
    get_child_value(Node, 'IDENTIFIER', Name).
convert_atom(Node, number(Value)) :-
    get_child_value(Node, 'INTEGER', ValueAtom),
    atom_number(ValueAtom, Value).

% Function call conversion
convert_function_call(Node, func_call(Name, Args)) :-
    get_child_value(Node, 'IDENTIFIER', Name),
    get_children_of_type(Node, 'ExpressionContext', ArgNodes),
    convert_expression_list(ArgNodes, Args).

% Expression list conversion
% (Remove duplicate definitions if already defined elsewhere)
% (Ensure that the clauses are together or declare them as discontiguous)
% Assuming the helper is defined, we don't redefine it here.

%% END OF CONVERSION.pl FILE
