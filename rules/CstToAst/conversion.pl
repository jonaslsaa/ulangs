% CST to AST conversion rules
%% START OF CONVERSION.pl FILE

%% conversion.pl

% Basic node structure interface
node_type(NodeId, Type) :- 
    node(NodeId, Type).

node_text(NodeId, Text) :- 
    has_value(NodeId, Text).

node_children(NodeId, Children) :- 
    findall(Child, has_child(NodeId, Child), Children).

% Required facts - these would typically be generated
:- dynamic node/2.
:- dynamic has_child/2.
:- dynamic has_value/2.
:- dynamic node_location/5.

% Sample data - this would be generated
% Function definition
node(1, 'ProgramContext').
node(2, 'FunctionDefContext').
node(3, 'IDENTIFIER').
has_value(3, 'fibonacci').
node_location(3, '__file__', 1, 4, 9).
has_child(1, 2).
has_child(2, 3).

% Parameter
node(4, 'ParamContext').
node(5, 'IDENTIFIER').
has_value(5, 'n').
node_location(5, '__file__', 1, 14, 1).
has_child(2, 4).
has_child(4, 5).

% Variable assignment
node(6, 'AssignmentContext').
node(7, 'IDENTIFIER').
has_value(7, 'result').
node_location(7, '__file__', 8, 0, 6).
has_child(1, 6).
has_child(6, 7).

% References to 'n'
node(8, 'AtomContext').
node(9, 'IDENTIFIER').
has_value(9, 'n').
node_location(9, '__file__', 2, 7, 1).
has_child(8, 9).

node(10, 'AtomContext').
node(11, 'IDENTIFIER').
has_value(11, 'n').
node_location(11, '__file__', 3, 12, 1).
has_child(10, 11).

node(12, 'AtomContext').
node(13, 'IDENTIFIER').
has_value(13, 'n').
node_location(13, '__file__', 5, 22, 1).
has_child(12, 13).

node(14, 'AtomContext').
node(15, 'IDENTIFIER').
has_value(15, 'n').
node_location(15, '__file__', 5, 41, 1).
has_child(14, 15).

% Reference to 'result'
node(16, 'AtomContext').
node(17, 'IDENTIFIER').
has_value(17, 'result').
node_location(17, '__file__', 9, 6, 6).
has_child(16, 17).

% Add nodes to program structure
has_child(1, 8).
has_child(1, 10).
has_child(1, 12).
has_child(1, 14).
has_child(1, 16).


%% END OF CONVERSION.pl FILE
