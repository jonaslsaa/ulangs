export const grammarGenerationDeveloperMessage = `ANTLR (Another Tool for Language Recognition) uses **context-free grammars (CFGs)** to define languages and generate parsers. ANTLR4 specifically employs **LL(*) parsing**—a variation of **LL(k)** parsers that can handle arbitrary lookahead. Here’s a breakdown of the logic required to follow ANTLR4 specs:

### 1. **Understanding LL(*) Parsers:**
- **Top-down** parsing: ANTLR generates parsers that parse input from the top-most rule (start rule) down to the leaves.
- **Predictive parsing**: It uses lookahead to choose between alternatives, avoiding backtracking when possible.
- **Non-determinism handling**: ANTLR resolves ambiguities by analyzing the grammar and employing **adaptive lookahead**.

### 2. **Grammar Structure (EBNF-like):**
- **Lexer rules** (Uppercase names): Define tokens by matching regular expressions.
- **Parser rules** (Lowercase names): Define how tokens combine to form higher-level constructs.
- **Actions**: Embedded code (usually in Java or other target languages) executes within the grammar.

antlr
grammar Expr;

expr: expr ('*'|'/') expr  # MulDiv
    | expr ('+'|'-') expr  # AddSub
    | INT                  # Int
    | '(' expr ')'         # Parens
    ;

INT: [0-9]+;
WS: [ \t\r\n]+ -> skip;


### 3. **Parse Tree Logic:**
- ANTLR builds a **parse tree** where each node corresponds to a grammar rule.
- The parser recursively descends through the tree, applying rules and matching tokens.

### 4. **Backtracking and Prediction:**
- ANTLR4 uses **prediction modes** (SLL, LL) to manage parsing paths.
- **Ambiguity resolution**: It prefers the longest match or the first alternative.

### 5. **Code Generation:**
- ANTLR generates parsers in languages like Java, Python, C#, and Go.
- The generated code implements listener/visitor patterns to traverse parse trees.

### Key Concepts to Master:
- **Tokenization** (lexer rules)
- **Recursive descent parsing** (parser rules)
- **Left-recursion elimination** (ANTLR4 supports direct left-recursion)
- **Error handling and recovery** (custom error strategies)
- **Semantic predicates** (condition parsing on semantic checks)
Ensuring all of this is correct when building a grammar is a challenge and requires extreme attention to detail.

Task:
From code snippet files, create the full ANTLR (antlr4) grammar to fully parse this programming language. Make sure all logic adds up and originates from the start rule. Create both a MyLexer.g4 and MyParser.g4 file. Start rule must be called "program". Make it somewhat generic as these files are just examples. Check that the grammar works for each file given at least. Don't add comment unless needed for an expert to understand the logic.
Always output the WHOLE and full grammar for both lexer and parser, within blocks like this  \`\`\`antlr `;

// System message
export const adapterGenerationMessage = `Your goal is to generate a robust, correct, and optimal Prolog adapter that transforms a Concrete Syntax Tree (CST) into an Abstract Syntax Tree (AST), accurately extracting symbol information (declarations and references) suitable for Language Server Protocol (LSP) symbol listing.

## Adapter Goals
Your adapter MUST accomplish the following precisely and reliably:

- **Declarations:**  
  Identify exactly which CST nodes represent symbol **declarations**. A declaration introduces a new named entity in the source code (variables, parameters, functions, classes, etc.).

- **References:**  
  Identify exactly which CST nodes represent **references** to already-declared symbols. A reference mentions an existing symbol without redeclaring it.

- **Uniqueness:**  
  Each declaration should appear **once** and should link accurately to all its corresponding references. No declaration duplicates should appear.

- **Ordering and Scoping:**  
  Respect source-file ordering. A symbol is declared exactly once at the earliest occurrence, and all subsequent occurrences are references. If the language has special scoping rules (locals, globals, parameters, etc.), ensure your adapter logic correctly reflects those rules.

## Adapter Requirements
- You MUST only write within the provided WRITEABLE AREA.
- Output ONLY Prolog code. This code will be placed into an adapter file.
- Ensure that your code makes the provided Main query execute successfully and correctly.
- Do NOT emit incomplete or placeholder code—every emitted rule or predicate must have well-defined behavior and work as-is.

## Writing Instructions
- **Define helper predicates** first to simplify your main logic (e.g., predicates like \`earlier_in_file/2\`, \`is_declaration/3\`, \`is_reference/2\`).
- **Implement clear logic rules**: a node is either a declaration or a reference, never both.
- **Explicitly handle symbol kinds** such as variables (13), functions (12), parameters (variable or parameter kind), and others as relevant to the CST provided.
- Write your adapter incrementally and test your reasoning by carefully considering counterexamples (e.g., shadowed names, reassigned variables, nested scopes).

You MUST only write within the WRITEABLE AREA. Output only the adapter / WRITEABLE AREA.`;

// Given in inital guess generation (not used)
export const adapterTipSQLAsAnExample = `Focus on Clear Domain Mappings (using SQL as an example)

“CREATE TABLE should be recognized as a top-level structure (like a class).”
“ColumnDef should be recognized as a field/property.”
“Function calls in queries are recognized as function references.”
Provide a Single Generic Reference Rule

“Any IDENTIFIER node that is not declared by the above rules is automatically a reference.”
This ensures that references in SELECT, JOIN, UPDATE, DELETE are all picked up without enumerating them each.
Use Node Types from the Parse Tree

“Check the actual parser grammar (for SQL or your DSL) to see how CREATE TABLE is named (like CreateTableStmtContext). Make a rule specifically for that.”
“Similarly, see how function calls are recognized (FunctionCallContext) and create a rule for that.”
Keep It Simple & Modular

One rule for each distinct ‘declaration’ type.
A single reference rule that says ‘if it’s not a declaration, it’s a reference.’
Ensure Identifier Extraction

Always have a dedicated identifier_name/2 rule that reads the literal text from the parse tree node.

Example for SQL:
\`\`\`
% WRITEABLE AREA
% Capture table names in FROM clause as "variable".
declaration_node(Node, "variable", IdNode) :-
    has_type(Node, 'FromClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture column names in SELECT clause as "field".
declaration_node(Node, "field", IdNode) :-
    has_type(Node, 'IdentifierListContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture column names in WHERE clause as "field".
declaration_node(Node, "field", IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

% Capture string literals in WHERE clause as "string".
declaration_node(Node, "string", IdNode) :-
    has_type(Node, 'ValueContext'),
    get_child_with_type(Node, 'STRING_LITERAL', IdNode).

% ------------------------------------------------------------------------------
% 2) reference_node(+ASTNode, -IdNode)
% ------------------------------------------------------------------------------
% References are similar to declarations, since SQL is mostly usage-based.

reference_node(Node, IdNode) :-
    has_type(Node, 'SelectStmtContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'FromClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'IDENTIFIER', IdNode).

reference_node(Node, IdNode) :-
    has_type(Node, 'WhereClauseContext'),
    get_child_with_type(Node, 'STRING_LITERAL', IdNode).

% ------------------------------------------------------------------------------
% 3) identifier_name(+IdNode, -Name)
% ------------------------------------------------------------------------------
identifier_name(IdNode, Name) :-
    has_type(IdNode, 'IDENTIFIER'),
    value(IdNode, Name).

identifier_name(IdNode, Name) :-
    has_type(IdNode, 'STRING_LITERAL'),
    value(IdNode, Name).

% End of adapter
% ------------------------------------------------------------------------------
% [/WRITABLE]
\`\`\`
`;

// System message to score adapter output
export function adapterScoringMessage(snippet: string, adapterOutput: string): string {
    return `Score 0 to 100 how well the generated definitions and references is by looking at the code snippet it is based on. 100 = means that the definitions perfectly describe the code snippet. 0 = means that the output don't make ANY sense. 50 = means that the output is mostly correct, but there are some errors but should be passing.
You will also be given the AST as context.
Note: Ignore following: builtin functions, undefined symbols, etc.
Note: Ignore readability of the output, e.g., if the output is too verbose or too terse. The output has already passed the schema validation.
HEAVILY PENALIZE symbols, defintions and references that are missing, incorrect, or not relevant. BUT don't penalize impossible symbols to resolve by context.
* Do symbols/definitions have references?

Supply also a list of reasons/errors (empty list if there are none if **perfect**, it is used as feedback).
<CodeSnippet>
${snippet}
</CodeSnippet>
<GeneratedOutput>
${adapterOutput}
</GeneratedOutput>`;
}