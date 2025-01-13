% [WRITABLE]
% Start of the template, it must be implemented by user.
% ------------------------------------------------------------------------------
% LSP SymbolKind - Allowed Values
% ------------------------------------------------------------------------------
% The user must pick from this list for their declarations.
%
%   "file"        -> 1
%   "module"      -> 2
%   "namespace"   -> 3
%   "package"     -> 4
%   "class"       -> 5
%   "method"      -> 6
%   "property"    -> 7
%   "field"       -> 8
%   "constructor" -> 9
%   "enum"        -> 10
%   "interface"   -> 11
%   "function"    -> 12
%   "variable"    -> 13
%   "constant"    -> 14
%   "string"      -> 15
%   "number"      -> 16
%   "boolean"     -> 17
%   "array"       -> 18
%   "object"      -> 19
%   "key"         -> 20
%   "null"        -> 21
%   "enum_member" -> 22
%   "struct"      -> 23
%   "event"       -> 24
%   "operator"    -> 25
%   "type_parameter" -> 26
%
% NOTE:
% 1) The user *must* use these atoms exactly for declaration_node/3.
% 2) For convenience, we also define the mapping symbol_kind/2 in definitions_query.pl,
%    so it is recognized system-wide.

% ------------------------------------------------------------------------------
% 1) declaration_node(+ASTNode, -KindAtom, -IdNode)
%  - Node: AST/CST node representing a declaration
%  - KindAtom: One of the SymbolKind atoms from the list above (e.g. "function")
%  - IdNode: The node that holds the identifier
% ------------------------------------------------------------------------------
%
% EXAMPLE: A function declaration
%
% declaration_node(Node, "function", IdNode) :-
%     has_type(Node, 'FunctionDefContext'),
%     get_child_with_type(Node, 'IDENTIFIER', IdNode).

% EXAMPLE: A variable declaration
%
% declaration_node(Node, "variable", IdNode) :-
%     has_type(Node, 'VarDefContext'),
%     get_child_with_type(Node, 'IDENTIFIER', IdNode).
%

% TODO: Implement your grammar-specific rules below:

declaration_node(Node, "function", IdNode) :-
    has_type(Node, 'FunctionDefContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'ParamContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Add or modify as needed, but only use the SymbolKind atoms defined above.

% ------------------------------------------------------------------------------
% 2) reference_node(+ASTNode, -IdNode)
%   - Succeeds if ASTNode is a reference to a symbol's identifier
% ------------------------------------------------------------------------------
reference_node(Node, IdNode) :-
    (
        has_type(Node, 'ExpressionContext')
        ; has_type(Node, 'AtomContext')
        ; has_type(Node, 'FunctionCallContext')
    ),
    get_child_with_type(Node, 'IDENTIFIER', IdNode),
    \+ declaration_node(_, _, IdNode).
    % ^ We exclude the possibility of being itself a declaration

% ------------------------------------------------------------------------------
% 3) identifier_name(+IdNode, -Name)
%   - Extracts the textual name from an IDENTIFIER node (or similar)
% ------------------------------------------------------------------------------
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).

% Add any other logic to successfully answer the queries

% End of user adapter template
% [/WRITABLE]