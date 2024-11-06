import OpenAI from "openai";
import { loadOpenAIEnvVars, type OpenAIEnv } from './utils';
import { Stats } from "./grammar";

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
2. The opening block and code language, eg: \`\`\`antlr
3. The start of search block: <<<<<<< SEARCH
4. A contiguous chunk of lines to search for in the existing source code
5. The dividing line: =======
6. The lines to replace into the source code
7. The end of the replace block: >>>>>>> REPLACE
8. The closing block: \`\`\`fenc

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
\`\`\`python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
from flask import Flask
=======
import math
from flask import Flask
>>>>>>> REPLACE
\`\`\`

mathweb/flask/app.py
\`\`\`python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
def factorial(n):
    "compute factorial"

    if n == 0:
        return 1
    else:
        return n * factorial(n-1)

=======
>>>>>>> REPLACE
\`\`\`
mathweb/flask/app.py
python
<`+/*don't confuse the IDE*/`<<<<<< SEARCH
    return str(factorial(n))
=======
    return str(math.factorial(n))
>>>>>>> REPLACE
\`\`\`
`.trim()
    }
  ]

  private openai: OpenAI;
  private openaiEnv: OpenAIEnv;
  private messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = [];
  public doLogging: boolean = false;
  public doLoggingOfEdits: boolean = false;

  constructor(openaiEnv: OpenAIEnv, private fence: [string, string] = DiffCodeEditor.DEFAULT_FENCE) {
    this.openai = new OpenAI({
      baseURL: openaiEnv.baseUrl,
      apiKey: openaiEnv.apiKey,
    });
    this.openaiEnv = openaiEnv;
  }

  private countEditBlocks(content: string): number {
    const HEAD = /^<{5,9} SEARCH\s*$/;
    const lines = content.split('\n');
    return lines.filter(line => HEAD.test(line.trim())).length;
  }

  async edit(files: FileContext[], instruction: string): Promise<CodeEdit[]> {
    let prompt = this.buildPrompt(files, instruction);
    for (let i = 0; i < 3; i++) {
      const response = await this.callOpenAI(prompt);
      if (response) {
        if (this.doLogging) console.log(response);
        const expectedBlocks = this.countEditBlocks(response);
        const edits = this.parseEdits(files, response);

        if (edits.length === expectedBlocks && edits.length > 0) {
          return edits;
        }

        if (edits.length !== expectedBlocks) {
          console.warn(`Mismatch: ${edits.length} edits parsed but found ${expectedBlocks} edit blocks`);
          // TODO: consume the edits we can and only try again those who failed
          prompt = "The number of parsed edits doesn't match the number of edit blocks(```). Please provide properly formatted edits.";
        } else {
          console.warn('No edits generated, retrying...');
          prompt = "You didn't give me any properly formatted edits, please try again!";
        }
      }
    }
    throw new Error('Failed to generate edits');
  }

  private buildPrompt(files: FileContext[], instruction: string): string {
    const filesContext = files
      .map(f => `${f.path}:\n${this.fence[0]}\n${f.content}\n${this.fence[1]}`)
      .join('\n\n');

    return `Files:\n${filesContext}\n\nInstruction: ${instruction}`;
  }

  parseEdits(files: FileContext[], response: string): CodeEdit[] {
    if (this.doLoggingOfEdits) console.log("\nParsing edits from response length:", response.length);
    const edits: CodeEdit[] = [];
    const editBlocks = Array.from(this.findEditBlocks(files, response));
    if (this.doLoggingOfEdits) console.log("Found edit blocks:", editBlocks.length);

    for (const [path, original, updated] of editBlocks) {
      if (!path || !updated) continue;

      const file = files.find(f => f.path === path);
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

  private *findEditBlocks(files: FileContext[], content: string): Generator<[string | null, string, string?]> {
    const HEAD = /^<{5,9} SEARCH\s*$/;
    const DIVIDER = /^={5,9}\s*$/;
    const UPDATED = /^>{5,9} REPLACE\s*$/;

    if (this.doLoggingOfEdits) console.log("Starting findEditBlocks with content length:", content.length);
    const lines = content.split('\n');
    if (this.doLoggingOfEdits) console.log("Number of lines:", lines.length);
    let i = 0;
    let currentPath: string | undefined;

    while (i < lines.length) {
      const line = lines[i].trim();

      if (HEAD.test(line)) {
        if (this.doLoggingOfEdits) console.log("Found HEAD marker at line", i, ":", line);
        try {
          // Find the associated file path, handling cases where it might be with the code fence
          const searchLines = lines.slice(Math.max(0, i - 3), i)
            .filter(line => line.trim().length > 0)
            .reverse();
          if (this.doLoggingOfEdits) console.log("Looking for path in previous lines:", searchLines);

          const pathLine = searchLines.find(l => {
            // Try exact match first
            const exactMatch = files.some(f => f.path === l.trim());
            if (exactMatch) {
              if (this.doLoggingOfEdits) console.log("Found exact path match:", l.trim());
              return true;
            }
            // Try extracting path when it's on same line as code fence
            const parts = l.split('`');
            const hasPath = parts.length > 0 && files.some(f => f.path === parts[0].trim());
            if (hasPath) {
              if (this.doLoggingOfEdits) console.log("Found path in code fence line:", parts[0].trim());
            }
            return hasPath;
          });

          let path: string | undefined;
          if (pathLine) {
            // Extract path, handling both standalone and code-fence cases
            const parts = pathLine.split('`');
            path = parts[0].trim();
            if (this.doLoggingOfEdits) console.log("Extracted path:", path);
          }

          path = path || currentPath;
          if (!path) {
            if (this.doLoggingOfEdits) console.log("No path found, skipping block");
            i++;
            continue;
          }
          currentPath = path;

          // Collect original text
          const originalText: string[] = [];
          i++;
          if (this.doLoggingOfEdits) console.log("Starting to collect original text at line", i);
          while (i < lines.length && !DIVIDER.test(lines[i].trim())) {
            originalText.push(lines[i]);
            i++;
          }
          if (this.doLoggingOfEdits) console.log("Collected original text:", originalText.length, "lines");

          if (i >= lines.length) {
            if (this.doLoggingOfEdits) console.log("Reached end of file before finding divider");
            break;
          }
          i++;

          // Collect updated text
          const updatedText: string[] = [];
          if (this.doLoggingOfEdits) console.log("Starting to collect updated text at line", i);
          while (i < lines.length && !UPDATED.test(lines[i].trim())) {
            updatedText.push(lines[i]);
            i++;
          }
          if (this.doLoggingOfEdits) console.log("Collected updated text:", updatedText.length, "lines");

          if (this.doLoggingOfEdits) console.log("Yielding edit block for path:", path);
          if (this.doLoggingOfEdits) console.log("Original text length:", originalText.length);
          if (this.doLoggingOfEdits) console.log("Updated text length:", updatedText.length);
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
    if (this.doLoggingOfEdits) console.log("=== Debug replaceMostSimilarChunk ===");
    if (this.doLoggingOfEdits) console.log("Part to find:", JSON.stringify(part));
    if (this.doLoggingOfEdits) console.log("First 100 chars of whole:", JSON.stringify(whole.substring(0, 100)));
    if (this.doLoggingOfEdits) console.log("Replace with:", JSON.stringify(replace));

    // Normalize line endings and trim trailing whitespace
    const normalizeText = (text: string) => {
      return text.replace(/\r\n/g, '\n')
        .split('\n')
        .map(line => line.trimRight())
        .join('\n');
    };

    const normalizedWhole = normalizeText(whole);
    const normalizedPart = normalizeText(part);
    const normalizedReplace = normalizeText(replace);

    if (this.doLoggingOfEdits) console.log("Normalized part to find:", JSON.stringify(normalizedPart));
    if (this.doLoggingOfEdits) console.log("Normalized whole start:", JSON.stringify(normalizedWhole.substring(0, 100)));

    // First try exact match with normalized text
    const exactIndex = normalizedWhole.indexOf(normalizedPart);
    if (exactIndex !== -1) {
      if (this.doLoggingOfEdits) console.log("Found exact normalized match at index:", exactIndex);
      return whole.slice(0, exactIndex) + replace + whole.slice(exactIndex + part.length);
    }

    // If no exact match, try line-by-line matching
    const wholeLines = normalizedWhole.split('\n');
    const partLines = normalizedPart.split('\n');
    const replaceLines = normalizedReplace.split('\n');

    if (this.doLoggingOfEdits) console.log("Whole lines:", wholeLines.length);
    if (this.doLoggingOfEdits) console.log("Part lines:", partLines.length);
    if (this.doLoggingOfEdits) console.log("First few whole lines:", wholeLines.slice(0, 3));
    if (this.doLoggingOfEdits) console.log("Part lines to match:", partLines);

    // Try to find matching chunk with flexible whitespace
    for (let i = 0; i <= wholeLines.length - partLines.length; i++) {
      const chunk = wholeLines.slice(i, i + partLines.length);
      const matches = chunk.every((line, j) => {
        const normalizedLine = line.trim();
        const normalizedPartLine = partLines[j].trim();
        return normalizedLine === normalizedPartLine;
      });

      if (matches) {
        if (this.doLoggingOfEdits) console.log("Found match with flexible whitespace at line:", i);
        // Preserve original indentation
        const indent = wholeLines[i].match(/^\s*/)?.[0] || '';
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

    if (this.doLoggingOfEdits) console.log("No match found");
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

    if (this.doLogging) console.log(messagesWithLatestPrompt.length, "messages:");
    if (this.doLogging) console.log(messagesWithLatestPrompt);

    Stats.addRequest();

    const completion = await this.openai.chat.completions.create({
      model: this.openaiEnv.model,
      messages: messagesWithLatestPrompt,
      max_tokens: 4096,
      temperature: 0.7,
    });

    if (completion.usage) Stats.addCompletedRequest(completion.usage.prompt_tokens, completion.usage.completion_tokens);

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

  applyEdits(files: FileContext[], edits: CodeEdit[]): FileContext[] {
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
    for (const file of files) {
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

    // Turn results into a file context
    return Object.entries(results).map(([path, content]) => ({
      path,
      content
    }));
  }

  async editAndApply(files: FileContext[], instruction: string) {
    const edits = await this.edit(files, instruction);
    return this.applyEdits(files, edits);
  }
}

export { DiffCodeEditor };


async function testEditParser() {
  const source = `
lexer grammar MyLexer;

// Keywords
SELECT: 'SELECT';
FROM: 'FROM';
WHERE: 'WHERE';
JOIN: 'JOIN';
ON: 'ON';
UPDATE: 'UPDATE';
SET: 'SET';
INSERT: 'INSERT';
INTO: 'INTO';
VALUES: 'VALUES';
CREATE: 'CREATE';
TABLE: 'TABLE';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
VARCHAR: 'VARCHAR';
INT: 'INT';
DATE: 'DATE';
PRIMARY: 'PRIMARY';
KEY: 'KEY';
FOREIGN: 'FOREIGN';
REFERENCES: 'REFERENCES';
AUTO_INCREMENT: 'AUTO_INCREMENT';
NOT: 'NOT';
NULL: 'NULL';

// Symbols
STAR: '*';
DOT: '.';
COMMA: ',';
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
EQ: '=';
NEQ: '!=';
LT: '<';
GT: '>';
LTE: '<=';
GTE: '>=';

// Literals
STRING_LITERAL: '\\'' (~'\\'')* '\\'';
DATE_LITERAL: '\\'' DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT '\\'';
NUMBER: DIGIT+;
fragment DIGIT: [0-9];

// Identifiers
IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;

// Whitespace
WS: [ \\t\\r\
]+ -> skip;
`.trim();

  const files = [{
    path: 'lexer.g4',
    content: source
  }];
  const editor = new DiffCodeEditor(loadOpenAIEnvVars());
  editor.doLogging = true;

  const mockResponse = `
The errors indicate that we need to define the tokens AVG, COUNT, AS, DELETE, GROUP, and BY in the lexer grammar. Let's add these tokens to the lexer grammar.

lexer.g4
\`\`\`antlr
<<<<<<< SEARCH
// Keywords
SELECT: 'SELECT';
FROM: 'FROM';
WHERE: 'WHERE';
JOIN: 'JOIN';
ON: 'ON';
UPDATE: 'UPDATE';
SET: 'SET';
INSERT: 'INSERT';
INTO: 'INTO';
VALUES: 'VALUES';
CREATE: 'CREATE';
TABLE: 'TABLE';
=======
// Keywords
SELECT: 'SELECT';
FROM: 'FROM';
WHERE: 'WHERE';
JOIN: 'JOIN';
ON: 'ON';
UPDATE: 'UPDATE';
SET: 'SET';
INSERT: 'INSERT';
INTO: 'INTO';
VALUES: 'VALUES';
CREATE: 'CREATE';
TABLE: 'TABLE';
DELETE: 'DELETE';
GROUP: 'GROUP';
BY: 'BY';
AVG: 'AVG';
COUNT: 'COUNT';
AS: 'AS';
>>>>>>> REPLACE
\`\`\`

This change adds all the missing token definitions to the lexer grammar. The tokens are:
1. DELETE - for DELETE statements
2. GROUP - for GROUP BY clauses
3. BY - for GROUP BY clauses
4. AVG - for average aggregate function
5. COUNT - for count aggregate function
6. AS - for column aliases

Now all tokens used in the parser grammar are properly defined in the lexer grammar, which should resolve the warnings.
`.trim();

  const edits = editor.parseEdits(files, mockResponse);
  console.log(edits.length, "edits generated");
  console.log(edits);

  // Apply the edits
  const results = editor.applyEdits(files, edits);
  console.log("Results:");
  results.forEach(({path, content}) => {
    console.log(`<${path}>\n${content.trim()}\n</${path}>\n`);
  });
}