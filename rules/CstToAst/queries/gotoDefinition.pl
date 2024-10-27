% Find function definition for a function call
find_function_definition(CallNode, Definition) :-
    % First, ensure we're looking at a function call
    has_type(CallNode, 'FunctionCallContext'),
    % Get the function name from the call
    get_child_value(CallNode, 'ID', FuncName),
    % Find the function definition in the program
    find_function_by_name(FuncName, Definition).

% Helper to find function definition by name
find_function_by_name(FuncName, Definition) :-
    % Start from program root (node 1)
    node(1, 'ProgramContext'),
    % Find function declaration
    child(1, StmtNode),
    has_type(StmtNode, 'StatementContext'),
    child(StmtNode, FuncNode),
    has_type(FuncNode, 'FunctionContext'),
    get_child_value(FuncNode, 'ID', FuncName),
    % Collect all relevant information
    collect_function_info(FuncNode, Definition).

% Collect complete function information
collect_function_info(FuncNode, function_def(Name, Params, Location, Body)) :-
    % Get function name
    get_child_value(FuncNode, 'ID', Name),
    % Get parameters
    first_child_of_type(FuncNode, 'ParamsContext', ParamsNode),
    collect_params(ParamsNode, Params),
    % Get location information
    node_location(FuncNode, File, Line, Col, Len),
    Location = location(File, Line, Col, Len),
    % Get function body
    first_child_of_type(FuncNode, 'FunctionBodyContext', BodyNode),
    collect_body(BodyNode, Body).

% Collect parameter information
collect_params(ParamsNode, Params) :-
    findall(Param, (
        get_child_with_type(ParamsNode, 'ID', ParamNode),
        value(ParamNode, Param)
    ), Params).

% Collect body information
collect_body(BodyNode, Body) :-
    first_child_of_type(BodyNode, 'IndentedStatementContext', IndentNode),
    first_child_of_type(IndentNode, 'ReturnStatementContext', ReturnNode),
    first_child_of_type(ReturnNode, 'ExpressionContext', ExprNode),
    convert_expression(ExprNode, Body).


% Main query
:- convert_cst_to_ast(1, AST),
    find_function_definition(CallNode, Definition),
    write_canonical(Definition),
    nl,
    halt.