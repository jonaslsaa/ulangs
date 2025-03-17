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


export const adapterGenerationMessage = `Your goal is to analyze a CST and convert it into an AST while also extracting symbol information (declarations and references) from a given Prolog representation of a parsed source code file.
I will give you many source code files and your goal is to find a optimal adapter that can transform all code correctly.
You will develop an adapter that sits in the middle of the file. Make sure this adapter will make the Main query run successfully with correct output.
You MUST only write within the WRITEABLE AREA. Output only the adapter / WRITEABLE AREA.`;

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

Always have a dedicated identifier_name/2 rule that reads the literal text from the parse tree node.`;

export function adapterScoringMessage(snippet: string, adapterOutput: string): string {
    return `Score 0 to 100 how well the definitions and AST tree generated is by looking at the code snippet it is based on. 100 = means that the definitions and AST tree perfectly describe the code snippet. 0 = means that the output don't make ANY sense.
Note: Ignore following: builtin functions, undefined symbols, etc.
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