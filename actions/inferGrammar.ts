import path from "path";
import type { CLIInferGrammarArguments } from "../cli";
import fs from 'fs';
import { calculateComplexity } from "../heuristics/complexity";
import { loadOpenAIEnvVars, type OpenAIEnv } from "../llm/utils";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { generateInitalGuess, Stats, type Grammar, type Snippet } from '../llm/grammar';
import { checkGrammar } from "../syntactic/check-grammar";
import { DiffCodeEditor } from "../llm/diff-editor";

function findAllCodeFiles(directory: string, extension: string, recursive: boolean): string[] {
    if (extension.startsWith('.')) {
        extension = extension.substring(1);
    }

    const files = fs.readdirSync(directory); // Get all files in the directory
    const codeFiles = files.filter(file => file.endsWith(`.${extension}`)); // Filter for code files

    if (recursive) { // If recursive, search subdirectories
        const subdirectories = files.filter(file => fs.statSync(path.join(directory, file)).isDirectory()); // Get subdirectories
        subdirectories.forEach(subdir => { // Iterate over subdirectories and search for code files
            codeFiles.push(...findAllCodeFiles(path.join(directory, subdir), extension, recursive));
        });
    }
    return codeFiles;
}

function loadFilesToSnippets(files: string[]): Snippet[] {
    return files.map(file => {
        const fileContent = fs.readFileSync(file, 'utf8');
        return {
            snippet: fileContent,
            fileName: path.basename(file),
            filePath: file
        };
    });
}

function sortByComplexity(snippets: Snippet[]): Snippet[] {
    // Sort snippets by complexity (ascending)
    return snippets.sort((a, b) => calculateComplexity(b.snippet) - calculateComplexity(a.snippet)).reverse();
}

function createTemporaryDirectory(name: string): string {
    const baseRelPath = path.join('.grammar-tmp');
    const basePath = path.join(process.cwd(), baseRelPath);
    if (!fs.existsSync(basePath)) {
        fs.mkdirSync(basePath);
    }
    const tempPath = path.join(basePath, name);
    if (!fs.existsSync(tempPath)) {
        fs.mkdirSync(tempPath);
    }
    return tempPath;
}

type TestedGrammar = {
    grammar: Grammar;
    errors?: ANTLRError[];
    success: boolean;
};

async function testGrammar(grammar: Grammar, snippet: Snippet): Promise<TestedGrammar> {
    // try to build the grammar 
    // by creating temporary directory
    // write grammars to file
    // run antlr4 to build the grammar (.ts files)

    // try to parse the snippet to a tree
    // by importing newly compiled .ts file
    // trying to parse the snippet to a tree
    // check if errorlistener has errors
    // return result
    const testedGrammar: TestedGrammar = {
        grammar,
        success: false,
    };

    const timestamp = new Date().toISOString().replaceAll(':', '-');
    const tempPath = createTemporaryDirectory(timestamp);

    const lexerFilePath = path.join(tempPath, 'MyLexer.g4');
    const parserFilePath = path.join(tempPath, 'MyParser.g4');
    const codeSnippetFilePath = path.join(tempPath, 'snippet.txt');

    fs.writeFileSync(lexerFilePath, grammar.lexerSource);
    fs.writeFileSync(parserFilePath, grammar.parserSource);
    fs.writeFileSync(codeSnippetFilePath, snippet.snippet);

    await new Promise(resolve => setTimeout(resolve, 500));
    try {
        testedGrammar.errors = await checkGrammar(lexerFilePath, parserFilePath, codeSnippetFilePath);
    } catch (error: any) {
        console.error("Failed to check grammar:", error);
        testedGrammar.errors = [{
            grammarType: 'UNKNOWN',
            source: 'BUILD',
            message: error['message'],
            file: lexerFilePath,
            line: undefined,
            column: undefined,
        }];
    }
    if (testedGrammar.errors.length > 0) {
        //console.log("Errors found in grammar:", testedGrammar.errors);
        return testedGrammar;
    }

    // console.log(` - ${lexerFilePath}\n - ${parserFilePath}`);
    testedGrammar.success = true;
    return testedGrammar;
}

async function testGrammarOnMany(grammar: Grammar,
    newSnippet: Snippet,
    previousSnippets: Snippet[]): Promise<TestedGrammar> {
    let numberOfTestsPassed = 0;
    let numberOfTestsTotal = 1 + previousSnippets.length;
    // Test first on new snippets
    const TestedGrammarMain = await testGrammar(grammar, newSnippet);
    if (!TestedGrammarMain.success) {
        console.log(`[FAILED] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests. Failed on ${newSnippet.fileName}!`);
        return TestedGrammarMain;
    }
    numberOfTestsPassed++;
    // Now test all previous snippets
    const TestedGrammarPrevious = await Promise.all(previousSnippets.map(async previousSnippet => {
        const TestedGrammarPrevious = await testGrammar(grammar, previousSnippet);
        // TODO: do something with the history/messages?
        return TestedGrammarPrevious;
    }));
    // If any previous snippets failed, return the first failed snippet
    for (const TestedGrammar of TestedGrammarPrevious) {
        if (!TestedGrammar.success) {
            console.log(`[FAILED] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests."`);
            return TestedGrammar;
        }
        numberOfTestsPassed++;
    }
    // If all previous snippets succeeded, return the main snippet
    console.log(`[PASS] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests!`);
    return TestedGrammarMain;
}

function getLineOfSource(source: string, lineNumber: number): string {
    const lines = source.split('\n');
    if (lineNumber < 1 || lineNumber > lines.length) {
        throw new Error(`Invalid line number: ${lineNumber}`);
    }
    return lines[lineNumber - 1];
}

function errorToString(error: ANTLRError, lexerSource: string, parserSource: string, codeSnippet: string, showGrammarType: boolean = true): string {
    const msg = error.message.replaceAll('\n', ' ');
    const firstPart = error.source === 'BUILD' ? 'While building' : 'Under parsing';
    const grammarPart = showGrammarType ? ` the ${error.grammarType.toLowerCase()} grammar` : '';
    let onLine = '';
    if (error.line) {
        let sourceCodeLine = '';
        if (error.source === 'BUILD' && error.grammarType === 'LEXER') sourceCodeLine = getLineOfSource(lexerSource, error.line);
        if (error.source === 'BUILD' && error.grammarType === 'PARSER') sourceCodeLine = getLineOfSource(parserSource, error.line);
        if (error.source === 'RUNTIME') sourceCodeLine = getLineOfSource(codeSnippet, error.line);
        onLine = ` on line ${error.line + 1}: \`${sourceCodeLine}\'`;
    }
    return `${firstPart}${grammarPart}${onLine} - ${msg}`;
}

async function repairGrammar(openaiEnv: OpenAIEnv, testedGrammar: TestedGrammar, snippet: Snippet, previousSnippets: Snippet[]): Promise<TestedGrammar | undefined> {
    const maxRetries = 12;
    const lexerFileNameConstant = 'lexer.g4';
    const parserFileNameConstant = 'parser.g4';
    let files = [
        { path: lexerFileNameConstant, content: testedGrammar.grammar.lexerSource },
        { path: parserFileNameConstant, content: testedGrammar.grammar.parserSource }
    ];
    const editor = new DiffCodeEditor(openaiEnv);
    editor.doLogging = true;

    let fixingSnippet = snippet;
    let errorsForFixingSnippet = [...(testedGrammar.errors ?? [])];
    const currentGrammar: Grammar = {
        lexerSource: testedGrammar.grammar.lexerSource,
        parserSource: testedGrammar.grammar.parserSource,
    }
    for (let i = 0; i < maxRetries; i++) {
        const prompt = `
Identify issues and fix the ANTLR4 grammars (lexer and/or parser) to correctly parse the following code snippet:
\`\`\`
${fixingSnippet.snippet}
\`\`\`
Got errors:
${errorsForFixingSnippet.map(error => errorToString(error, currentGrammar.lexerSource, currentGrammar.parserSource, snippet.snippet)).join('\n')}

Start by listing the possible issues and solutions (write likelihood in braces, e.g., {likelihood}), then propose fixes to the files (using SEARCH/REPLACE blocks).
`;
        const edits = await editor.edit(files, prompt);
        console.log(edits.length, "edits generated");
        files = editor.applyEdits(files, edits);

        // TODO: check if the repaired files have actually been changed

        // Update the current grammar with the repaired files
        const [lexerFileContext, parserFileContext] = files;
        if (lexerFileContext) { // Lexer file
            currentGrammar.lexerSource = lexerFileContext.content;
        }
        if (parserFileContext) { // Parser file
            currentGrammar.parserSource = parserFileContext.content;
        }
        // Test new grammar on the main snippet, even if we are not repairing that specific snippet (see below)
        const newGrammar = await testGrammar(currentGrammar, snippet);
        if (!newGrammar.success) {
            console.warn(`(${snippet.fileName}) Failed to repair grammar, retrying...`);
            fixingSnippet = snippet;
            errorsForFixingSnippet = [...newGrammar.errors ?? []];
            continue;
        }
        // Now we check the rest of the snippets
        for (const previousSnippet of previousSnippets) {
            const newGrammar = await testGrammar(currentGrammar, previousSnippet);
            if (!newGrammar.success) {
                console.warn(`${previousSnippet.fileName}) Failed to repair (previous snippet) grammar, retrying...`);
                fixingSnippet = previousSnippet;
                errorsForFixingSnippet = [...newGrammar.errors ?? []];
            }
        }
        // If we reach this point, we have successfully repaired the grammar
        return {
            grammar: currentGrammar,
            errors: [],
            success: true,
        };
    }
}

const temporaryFileDirectoryRecords = new Set<string>();

function ExitAndLogStats(exitCode: number = 0) {
    // Remove temporary file directory
    for (const tempPath of temporaryFileDirectoryRecords) {
        fs.rmSync(tempPath, { recursive: true, force: true });
    }

    console.log("\n[Stats]");
    console.log(`Generated ${Stats.totalRequests} requests, and completed ${Stats.totalCompletedRequests} requests.`);
    console.log(`    Input tokens: ${Stats.inputTokens}, Output tokens: ${Stats.outputTokens}`);
    console.log(`    ${Stats.totalTokens} tokens (${Stats.avgTokensPerRequest} avg tokens per request)`);
    if (Stats.score.size > 0) console.log("\n[Model scores]");
    Array.from(Stats.score.entries())
        .sort(([, scoreA], [, scoreB]) => scoreB - scoreA)
        .forEach(([modelName, score]) => {
            console.log(`    - ${modelName}: ${score}`);
        });
    process.exit(exitCode);
}

function loadFile(filePath: string | undefined): string | undefined {
    if (filePath === undefined) return undefined;
    if (!fs.existsSync(filePath)) return undefined;
    console.log(`Loading initial grammar file from ${filePath}`);
    return fs.readFileSync(filePath, 'utf8');
}

async function buildFirstIntermediateSolution(openaiEnv: OpenAIEnv,
    initalLexer: string | undefined,
    initalParser: string | undefined,
    tryOneShot: boolean = false,
    snippets: Snippet[] = [],
    fileNamesThatDidntPass: string[] = []
): Promise<Grammar> {
    let g: Grammar = {
        lexerSource: 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE\n',
        parserSource: 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
    };
    if (initalLexer) g.lexerSource = initalLexer;
    if (initalParser) g.parserSource = initalParser;

    if (tryOneShot) {
        console.log("Generating initial guess for the first intermediate solution...");
        const newG = await generateInitalGuess(openaiEnv, snippets, g.lexerSource, g.parserSource, fileNamesThatDidntPass);
        g = newG;
    }

    return g;
}

async function checkGrammarOnMany(lexerPath: string, parserPath: string, codePaths: string[]): Promise<ANTLRError[]> {
    const results = await Promise.all(codePaths.map(codePath => checkGrammar(lexerPath, parserPath, codePath)));
    return results.flat();
}

function createTemporaryFile(dir: string, fileName: string): string {
    const tempPath = path.join(dir, '.tmp');
    if (!fs.existsSync(tempPath)) {
        fs.mkdirSync(tempPath);
    }
    temporaryFileDirectoryRecords.add(tempPath);
    const tempFilePath = path.join(tempPath, fileName);
    if (!fs.existsSync(tempFilePath)) {
        fs.writeFileSync(tempFilePath, '');
    }
    return tempFilePath;
}

async function loadSnippetsByComplexity(directory: string, extension: string, recursive: boolean) {
    const files = findAllCodeFiles(directory, extension, recursive);
    const snippets = loadFilesToSnippets(files.map(file => path.join(directory, file)));
    if (snippets.length === 0) {
        console.error('No files found with the specified extension!');
        return;
    }

    const sortedSnippets = sortByComplexity(snippets);

    const lowestComplexity = sortedSnippets[0];
    const highestComplexity = sortedSnippets[sortedSnippets.length - 1];
    console.log(`Loaded ${files.length} files and sorted them by complexity: ${calculateComplexity(lowestComplexity.snippet)} to ${calculateComplexity(highestComplexity.snippet)}`);
    console.log(`Analysing the files in the following order: ${sortedSnippets.map(snippet => snippet.fileName).join(', ')}`);
    
    return sortedSnippets;
}

export async function doInferGrammar(directory: string, extension: string, outputDir: string, options: CLIInferGrammarArguments) {
    // const maxRetries = 3;

    const sortedSnippets = await loadSnippetsByComplexity(directory, extension, options.recursive);
    if (sortedSnippets === undefined || sortedSnippets.length === 0) {
        console.error('No files found with the given extension.');
        return;
    }

    // Create temporary lexer and parser files
    const tempLexerFilePath = createTemporaryFile(outputDir, 'MyLexer.tmp.g4');
    const tempParserFilePath = createTemporaryFile(outputDir, 'MyParser.tmp.g4');

    // Load OpenAI environment variables
    const openaiEnv = loadOpenAIEnvVars();

    // Load initial lexer and parser if they exist
    const initialLexer = loadFile(options.initialLexer);
    const initialParser = loadFile(options.initialParser);

    // If both initial lexer and parser are provided, let's just check if they already are syntactically valid
    let fileNamesThatDidntPass: Set<string> = new Set();
    if (initialLexer && initialParser && options.initialLexer && options.initialParser) {
        const errors = await checkGrammarOnMany(options.initialLexer, options.initialParser, sortedSnippets.map(snippet => snippet.filePath));
        if (errors.length === 0) {
            console.log("Loaded grammar (lexer and parser) is syntactically valid! Exiting...");
            process.exit(0);
        } else {
            console.error("Loaded grammar (lexer and parser) are NOT syntactically valid!");
        }
        errors.forEach(error => error.file && fileNamesThatDidntPass.add(error.file));
    }

    // Build our first intermediate solution.
    // Static initialization:
    //      1. Empty grammar (just a template)
    //      2. The initial lexer and parser
    // Dynamic initialization:
    //      1. Guess based on all the snippets
    //      2. Guess based on the snippets and the initial lexer and parser
    const snippetsUsedInGuess = sortedSnippets.map(snippet => snippet); // TODO: shouldn't be all files ...
    let currentIntermediateSolution = await buildFirstIntermediateSolution(openaiEnv,
        initialLexer,
        initialParser,
        !options.skipFirstGuess,
        snippetsUsedInGuess,
        [...fileNamesThatDidntPass]
    );

    const snippetHistory: Snippet[] = [];
    let didWeGiveUp = false;
    let lastSnippetBeforeGiveUp: Snippet | undefined = undefined;

    for (const snippet of sortedSnippets) {
        // Write grammar to temporary files (lexer and parser)
        fs.writeFileSync(tempLexerFilePath, currentIntermediateSolution.lexerSource);
        fs.writeFileSync(tempParserFilePath, currentIntermediateSolution.parserSource);

        // Test current solution against this snippet and history
        let testedGrammar = await testGrammarOnMany(currentIntermediateSolution, snippet, snippetHistory);

        if (!testedGrammar.success) {
            console.log("Current solution failed tests, attempting repair...");

            // Try to repair the grammar
            const repairedGrammar = await repairGrammar(openaiEnv, testedGrammar, snippet, snippetHistory);

            if (repairedGrammar?.success) {
                console.log("Successfully repaired grammar");
                currentIntermediateSolution = repairedGrammar.grammar;
                testedGrammar = repairedGrammar;
            } else {
                throw new Error("Failed to repair grammar");
                // TODO: handle this case
            }
        }

        snippetHistory.push(snippet);
        console.log(`[New intermediate solution] Inferred grammar from ${snippet.fileName} (complexity=${calculateComplexity(snippet.snippet)})`);
    }

    const finalGrammar = currentIntermediateSolution;
    if (finalGrammar === undefined) {
        console.error("Failed to find ANY valid grammar!");
        ExitAndLogStats(1);
        return;
    }

    if (didWeGiveUp) {
        console.log("Couldn't pass following snippets:", [...snippetHistory, lastSnippetBeforeGiveUp!].map(snippet => snippet.fileName).join(', '));
        console.error("Failed to find a final grammar.");
        ExitAndLogStats(1);
    } else {
        console.log("A final solution found with complexity", calculateComplexity(finalGrammar.lexerSource + finalGrammar.parserSource));
    }

    // Write to current directory
    const outputLexerFilePath = path.join(outputDir, 'MyLexer.g4');
    const outputParserFilePath = path.join(outputDir, 'MyParser.g4');
    fs.writeFileSync(outputLexerFilePath, finalGrammar.lexerSource);
    fs.writeFileSync(outputParserFilePath, finalGrammar.parserSource);
    console.log("Wrote final grammar to", outputLexerFilePath, "and", outputParserFilePath);
    ExitAndLogStats();
}

export async function doVerboseCheck(directory: string, extension: string, lexerPath: string, parserPath: string) {
    // Check all files in directory with given extension are successfully parsed by the antlr grammar given
    const snippets = await loadSnippetsByComplexity(directory, extension, true);
    if (snippets === undefined || snippets.length === 0) {
        console.error('No files found with the given extension.');
        return;
    }

    // Check grammars
    let fileNamesThatDidntPass: Set<string> = new Set();
    const errors = await checkGrammarOnMany(lexerPath, parserPath, snippets.map(snippet => snippet.filePath));
    if (errors.length === 0) {
        console.log("Loaded grammar (lexer and parser) is syntactically valid!");
        return;
    } else {
        console.error("Loaded grammar (lexer and parser) are NOT syntactically valid!");
        console.log("=== Errors ===");
        console.log(errors);
    }

    errors.forEach(error => error.file && fileNamesThatDidntPass.add(error.file));
    console.log(`The following files did not pass the grammar check: ${[...fileNamesThatDidntPass].join(', ')}`);
    process.exit(1);
}