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