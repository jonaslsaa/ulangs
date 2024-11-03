import OpenAI from "openai";
import { loadOpenAIEnvVars, type OpenAIEnv } from './utils';

interface CodeEdit {
  path: string;
  original: string;
  updated: string;
  response: string;
}

interface FileContext {
  path: string;
  content: string;
}

class DiffCodeEditor {
  private static readonly DEFAULT_FENCE: [string, string] = ['```', '```'];

  private static readonly PROMPT = `Act as an expert software developer.
Always use best practices when coding.
You will receive code files and modify them according to instructions.

Every edit must use this format:
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
[code to find]
=======
[code to replace with]
>>>>>>> REPLACE

# *SEARCH/REPLACE block* Rules:

Every *SEARCH/REPLACE block* must use this format:
1. The *FULL* file path alone on a line, verbatim. No bold asterisks, no quotes around it, no escaping of characters, etc.
2. The opening fence and code language, eg: {fence[0]}python
3. The start of search block: <<<<<<< SEARCH
4. A contiguous chunk of lines to search for in the existing source code
5. The dividing line: =======
6. The lines to replace into the source code
7. The end of the replace block: >>>>>>> REPLACE
8. The closing fence: {fence[1]}

Use the *FULL* file path, as shown to you by the user.

Every *SEARCH* section must *EXACTLY MATCH* the existing file content, character for character, including all comments, docstrings, etc.
If the file contains code or other data wrapped/escaped in json/xml/quotes or other containers, you need to propose edits to the literal contents of the file, including the container markup.

*SEARCH/REPLACE* blocks will *only* replace the first match occurrence.
Including multiple unique *SEARCH/REPLACE* blocks if needed.
Include enough lines in each SEARCH section to uniquely match each set of lines that need to change.

Keep *SEARCH/REPLACE* blocks concise.
Break large *SEARCH/REPLACE* blocks into a series of smaller blocks that each change a small portion of the file.
Include just the changing lines, and a few surrounding lines if needed for uniqueness.
Do not include long runs of unchanging lines in *SEARCH/REPLACE* blocks.

Only create *SEARCH/REPLACE* blocks for files that the user has added to the chat!

To move code within a file, use 2 *SEARCH/REPLACE* blocks: 1 to delete it from its current location, 1 to insert it in the new location.
`
  private static readonly EXAMPLES: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = [
    {
      role: 'user',
      content: `Change get_factorial() to use math.factorial`
    },
    {
      role: 'assistant',
      content: `To make this change we need to modify \`mathweb/flask/app.py\` to:

1. Import the math package.
2. Remove the existing factorial() function.
3. Update get_factorial() to call math.factorial instead.

Here are the *SEARCH/REPLACE* blocks:

mathweb/flask/app.py
{fence[0]}python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
from flask import Flask
=======
import math
from flask import Flask
>>>>>>> REPLACE
{fence[1]}

mathweb/flask/app.py
{fence[0]}python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
def factorial(n):
    "compute factorial"

    if n == 0:
        return 1
    else:
        return n * factorial(n-1)

=======
>>>>>>> REPLACE
{fence[1]}

mathweb/flask/app.py
{fence[0]}python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
    return str(factorial(n))
=======
    return str(math.factorial(n))
>>>>>>> REPLACE
{fence[1]}
`}
  ]

  private openai: OpenAI;
  private openaiEnv: OpenAIEnv;
  private messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = [];
  public doLogging: boolean = false;

  constructor(private files: FileContext[], openaiEnv: OpenAIEnv, private fence: [string, string] = DiffCodeEditor.DEFAULT_FENCE) {
    this.openai = new OpenAI({
      baseURL: openaiEnv.baseUrl,
      apiKey: openaiEnv.apiKey,
    });
    this.openaiEnv = openaiEnv;
  }

  async edit(instruction: string): Promise<CodeEdit[]> {
    const prompt = this.buildPrompt(instruction);
    for (let i = 0; i < 3; i++) {
      const response = await this.callOpenAI(prompt);
      if (response) {
        if (this.doLogging) console.log(response);
        return this.parseEdits(response);
      }
      console.warn('Failed to generate edits, retrying...');
    }
    throw new Error('Failed to generate edits');
  }

  private buildPrompt(instruction: string): string {
    const filesContext = this.files
      .map(f => `${f.path}:\n${this.fence[0]}\n${f.content}\n${this.fence[1]}`)
      .join('\n\n');

    return `Files:\n${filesContext}\n\nInstruction: ${instruction}`;
  }

  private parseEdits(response: string): CodeEdit[] {
    const edits: CodeEdit[] = [];
    const editBlocks = Array.from(this.findEditBlocks(response));

    for (const [path, original, updated] of editBlocks) {
      if (!path || !updated) continue;

      const file = this.files.find(f => f.path === path);
      if (!file) continue;

      const cleanOriginal = this.stripQuotedWrapping(original, path);
      const cleanUpdated = this.stripQuotedWrapping(updated, path);

      // Verify we can find and replace this edit
      const newContent = this.replaceMostSimilarChunk(
        file.content,
        cleanOriginal,
        cleanUpdated
      );
      if (newContent) {
        edits.push({
          path,
          original: cleanOriginal,
          updated: cleanUpdated,
          response: response
        });
      }
    }

    return edits;
  }

  private *findEditBlocks(content: string): Generator<[string | null, string, string?]> {
    const HEAD = /^<{5,9} SEARCH\s*$/;
    const DIVIDER = /^={5,9}\s*$/;
    const UPDATED = /^>{5,9} REPLACE\s*$/;

    const lines = content.split('\n');
    let i = 0;
    let currentPath: string | undefined;

    while (i < lines.length) {
      const line = lines[i].trim();

      if (HEAD.test(line)) {
        try {
          // Find the associated file path
          const pathLine = lines.slice(Math.max(0, i - 3), i)
            .reverse()
            .find(l => this.files.some(f => f.path === l.trim()));

          const path = pathLine?.trim() || currentPath;
          if (!path) {
            i++;
            continue;
          }
          currentPath = path;

          // Collect original text
          const originalText: string[] = [];
          i++;
          while (i < lines.length && !DIVIDER.test(lines[i].trim())) {
            originalText.push(lines[i]);
            i++;
          }

          if (i >= lines.length) break;
          i++;

          // Collect updated text
          const updatedText: string[] = [];
          while (i < lines.length && !UPDATED.test(lines[i].trim())) {
            updatedText.push(lines[i]);
            i++;
          }

          yield [path, originalText.join('\n'), updatedText.join('\n')];

        } catch (error) {
          console.error('Error parsing edit block:', error);
        }
      }
      i++;
    }
  }

  private stripQuotedWrapping(text: string, fname: string): string {
    let lines = text.split('\n');

    // Remove filename if present
    if (lines[0].trim().endsWith(fname)) {
      lines = lines.slice(1);
    }

    // Remove code fence if present
    if (
      lines[0].startsWith(this.fence[0]) &&
      lines[lines.length - 1].startsWith(this.fence[1])
    ) {
      lines = lines.slice(1, -1);
    }

    return lines.join('\n').trim() + '\n';
  }

  private replaceMostSimilarChunk(whole: string, part: string, replace: string): string | undefined {
    const [wholeContent, wholeLines] = this.prep(whole);
    const [, partLines] = this.prep(part);
    const [, replaceLines] = this.prep(replace);

    // Try exact match first
    let result = this.perfectReplace(wholeLines, partLines, replaceLines);
    if (result) return result;

    // Try matching with flexible whitespace
    result = this.replaceWithFlexibleWhitespace(wholeLines, partLines, replaceLines);
    if (result) return result;

    // Try matching with ellipsis
    try {
      result = this.replaceDotDotDot(whole, part, replace);
      if (result) return result;
    } catch (error) {
      // Continue to next approach
    }

    return undefined;
  }

  private prep(content: string): [string, string[]] {
    if (content && !content.endsWith('\n')) {
      content += '\n';
    }
    return [content, content.split('\n')];
  }

  private perfectReplace(
    wholeLines: string[],
    partLines: string[],
    replaceLines: string[]
  ): string | undefined {
    const partLen = partLines.length;

    for (let i = 0; i <= wholeLines.length - partLen; i++) {
      const chunk = wholeLines.slice(i, i + partLen);
      if (this.arraysEqual(chunk, partLines)) {
        return [
          ...wholeLines.slice(0, i),
          ...replaceLines,
          ...wholeLines.slice(i + partLen)
        ].join('\n');
      }
    }
    return undefined;
  }

  private replaceWithFlexibleWhitespace(
    wholeLines: string[],
    partLines: string[],
    replaceLines: string[]
  ): string | undefined {
    // Calculate common leading whitespace
    const leading = [
      ...partLines.filter(p => p.trim()).map(p => p.length - p.trimLeft().length),
      ...replaceLines.filter(p => p.trim()).map(p => p.length - p.trimLeft().length)
    ];

    if (leading.length && Math.min(...leading)) {
      const numLeading = Math.min(...leading);
      partLines = partLines.map(p => p.trim() ? p.slice(numLeading) : p);
      replaceLines = replaceLines.map(p => p.trim() ? p.slice(numLeading) : p);
    }

    // Try to match with flexible whitespace
    for (let i = 0; i <= wholeLines.length - partLines.length; i++) {
      const chunk = wholeLines.slice(i, i + partLines.length);
      if (chunk.every((line, j) => line.trim() === partLines[j].trim())) {
        const indent = chunk[0].match(/^\s*/)?.[0] || '';
        const indentedReplace = replaceLines.map(line =>
          line.trim() ? indent + line : line
        );

        return [
          ...wholeLines.slice(0, i),
          ...indentedReplace,
          ...wholeLines.slice(i + partLines.length)
        ].join('\n');
      }
    }

    return undefined;
  }

  private replaceDotDotDot(whole: string, part: string, replace: string): string | undefined {
    const dotsRegex = /^\s*\.\.\.\n/gm;

    const partPieces = part.split(dotsRegex);
    const replacePieces = replace.split(dotsRegex);

    if (partPieces.length !== replacePieces.length) {
      throw new Error("Unpaired ... in edit block");
    }

    if (partPieces.length === 1) {
      return undefined;
    }

    let result = whole;
    for (let i = 0; i < partPieces.length; i += 2) {
      const part = partPieces[i];
      const replace = replacePieces[i];

      if (!part && !replace) continue;

      if (!part && replace) {
        if (!result.endsWith('\n')) result += '\n';
        result += replace;
        continue;
      }

      const count = (result.match(new RegExp(this.escapeRegExp(part), 'g')) || []).length;
      if (count === 0) throw new Error("No match found");
      if (count > 1) throw new Error("Multiple matches found");

      result = result.replace(part, replace);
    }

    return result;
  }

  private escapeRegExp(string: string): string {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  private arraysEqual(a: string[], b: string[]): boolean {
    return a.length === b.length && a.every((val, i) => val === b[i]);
  }

  private async callOpenAI(prompt: string) {
    if (this.messages.length === 0) {
      this.messages.push({
        role: 'system',
        content: DiffCodeEditor.PROMPT
      });
      this.messages.push(...DiffCodeEditor.EXAMPLES);
    }

    const messagesWithLatestPrompt = [...this.messages];
    messagesWithLatestPrompt.push({
      role: 'user',
      content: prompt
    });

    if (this.doLogging) console.log(this.messages);

    const completion = await this.openai.chat.completions.create({
      model: this.openaiEnv.model,
      messages: messagesWithLatestPrompt,
      max_tokens: 4096,
      temperature: 0.7,
    });

    const responseContent = completion.choices[0].message.content;
    if (responseContent) { // seems like it worked
      // lets add the latest prompt to the messages and the response to the messages
      this.messages.push({
        role: 'user',
        content: prompt
      });
      this.messages.push({
        role: 'assistant',
        content: responseContent
      });
    }
    return responseContent;
  }

  applyEdits(edits: CodeEdit[]): Record<string, string> {
    const results: Record<string, string> = {};

    // Group edits by file
    const editsByFile = edits.reduce((acc, edit) => {
      if (!acc[edit.path]) {
        acc[edit.path] = [];
      }
      acc[edit.path].push(edit);
      return acc;
    }, {} as Record<string, CodeEdit[]>);

    // Apply edits for each file
    for (const file of this.files) {
      let content = file.content;
      const fileEdits = editsByFile[file.path] || [];

      // Apply each edit sequentially
      for (const edit of fileEdits) {
        const newContent = this.replaceMostSimilarChunk(
          content,
          edit.original,
          edit.updated
        );
        if (newContent) {
          content = newContent;
        } else {
          console.warn(`Failed to apply edit in ${file.path}`);
        }
      }

      results[file.path] = content;
    }

    return results;
  }

  async editAndApply(instruction: string): Promise<Record<string, string>> {
    const edits = await this.edit(instruction);
    return this.applyEdits(edits);
  }
}

export { DiffCodeEditor };

async function main() {
  const files = [{
    path: 'lexer.g4',
    content: `
lexer grammar MyLexer;

// Keywords
DEF: 'def';
RET: 'ret';
FOR: 'for';
IN: 'in';
IF: 'if';
ELSE: 'else';
PRINT: 'print';

// Types
INT_TYPE: 'int';
STRING_TYPE: 'string';
LIST_TYPE: 'List';

// Operators
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
ASSIGN: '=';
LE: '<=';

// Delimiters
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
COLON: ':';
COMMA: ',';
LT: '<';
GT: '>';

// Literals
INTEGER: [0-9]+;
STRING: '"' (~["\\r\\n])* '"';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Comments
COMMENT: '#' ~[\\r\\n]* -> skip;

// Whitespace
WS: [ \\t\\r\\n]+ -> skip;
`.trim()
  },
  {
    path: 'parser.g4',
    content: `
parser grammar MyParser;
options { tokenVocab=MyLexer; }

program
    : (statement | functionDef)*
    ;

functionDef
    : DEF IDENTIFIER LPAREN paramList? RPAREN typeAnnotation? COLON
      block
    ;

paramList
    : param (COMMA param)*
    ;

param
    : IDENTIFIER typeAnnotation?
    ;

typeAnnotation
    : LT type GT
    ;

type
    : INT_TYPE
    | STRING_TYPE
    | LIST_TYPE typeAnnotation
    | IDENTIFIER
    ;

block
    : statement+
    ;

statement
    : assignment
    | forLoop
    | ifStatement
    | functionCall COMMENT?
    | returnStatement
    | printStatement
    | COMMENT
    ;

assignment
    : IDENTIFIER typeAnnotation? ASSIGN expression
    ;

forLoop
    : FOR IDENTIFIER typeAnnotation? IN expression COLON
      block
    ;

ifStatement
    : IF expression COLON
      block
      (ELSE COLON block)?
    ;

returnStatement
    : RET typeAnnotation? expression
    ;

printStatement
    : PRINT LPAREN expression RPAREN
    ;

expression
    : MINUS expression
    | expression (MULT | DIV | MOD) expression
    | expression (PLUS | MINUS) expression
    | expression LE expression
    | LPAREN expression RPAREN
    | list
    | functionCall
    ;

functionCall
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

list
    : LBRACK (expression (COMMA expression)*)? RBRACK
    ;

atom
    : INTEGER
    | STRING
    | IDENTIFIER
    ;
`.trim()
  }
];

  const editor = new DiffCodeEditor(files, loadOpenAIEnvVars());
  editor.doLogging = false;

  const prompt = `
Identify and fix the ANTLR4 lexer and parser grammars to correctly parse the following code snippet:
\`\`\`
def fibonacci(n<int>)<int>:
    if n <= 1:
        ret n
    else:
        ret fibonacci(n - 1) + fibonacci(n - 2)

# Test the function
result<int> = fibonacci(10)
print(result)  # Expected output: 55
\`\`\`
Got errors:
Line 2: \`    if n <= 1:\` - mismatched input '<=' expecting '('
Line 2: \`    if n <= 1:\` - mismatched input '1' expecting {'-', '(', '[', IDENTIFIER}
Line 4: \`    else:\` - mismatched input 'else' expecting '('
`;

  const edits = await editor.edit(prompt);


  // The editor will handle:
  // - Matching with proper indentation
  // - Multiple edit blocks
  // - Partial matches with ...
  // - Validation of matches
  //console.log(edits);
  console.log(edits.length, "edits generated");

  // Apply the edits
  const results = editor.applyEdits(edits);
  Object.entries(results).forEach(([path, content]) => {
    console.log(`<${path}>\n${content.trim()}\n</${path}>`);
  });
}

main();
