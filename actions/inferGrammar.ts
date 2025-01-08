import path from "path";
import type { CLIInferGrammarArguments } from "../cli";
import fs from 'fs';
import { calculateComplexity } from "../heuristics/complexity";
import { loadOpenAIEnvVars, type OpenAIEnv, type OpenAIMessage } from "../llm/utils";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { generateInitalGuess, Stats, type Grammar, type Snippet, type TestedSnippet } from '../llm/grammar';
import { checkGrammar } from "../syntactic/check-grammar";

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

type Candidate = {
    grammar: Grammar;
    snippets: TestedSnippet[];
    score: number; // Number of successful tested grammars
}

async function testGrammar(grammar: Grammar, snippet: Snippet): Promise<TestedSnippet> {
    // try to build the grammar 
    // by creating temporary directory
    // write grammars to file
    // run antlr4 to build the grammar (.ts files)

    // try to parse the snippet to a tree
    // by importing newly compiled .ts file
    // trying to parse the snippet to a tree
    // check if errorlistener has errors
    // return result
    const testedSnippet: TestedSnippet = {
        onSnippet: snippet,
        usedGrammar: grammar,
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
    await new Promise(resolve => setTimeout(resolve, 250));

    try {
        testedSnippet.errors = await checkGrammar(lexerFilePath, parserFilePath, codeSnippetFilePath);
    } catch (error: any) {
        console.error("Failed to check grammar:", error);
        testedSnippet.errors = [{
            grammarType: 'UNKNOWN',
            source: 'BUILD',
            message: error['message'],
            file: lexerFilePath,
            line: undefined,
            column: undefined,
        }];
    }
    testedSnippet.success = testedSnippet.errors.length === 0;
    return testedSnippet;
}

async function testGrammarOnMany(grammar: Grammar,
        newSnippet: Snippet | undefined,
        previousSnippets: Snippet[],
        stopOnFirstFailure: boolean = false): Promise<TestedSnippet[]> {

    let numberOfTestsPassed = 0;
    let numberOfTestsTotal = 1 + previousSnippets.length;

    let newTested: TestedSnippet | undefined = undefined;
    if (newSnippet) {
        // Test first on new snippets
        newTested = await testGrammar(grammar, newSnippet);
        if (!newTested.success) {
            console.log(`[FAIL] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests. Failed on ${newSnippet.fileName}!`);
            if (stopOnFirstFailure) return [newTested];
        } else {
            numberOfTestsPassed++;
        }
    }

    // Now test all previous snippets
    if (stopOnFirstFailure) {
        for (const previousSnippet of previousSnippets) {
            const tested = await testGrammar(grammar, previousSnippet);
            if (tested.success) return [tested];
        }
    }
    // else, test all previous snippets in parallel
    const TestedPrevious = await Promise.all(previousSnippets.map(async previousSnippet => {
        return await testGrammar(grammar, previousSnippet);;
    }));
    // Count all the other tests
    for (const tested of TestedPrevious) {
        if (tested.success) {
            numberOfTestsPassed++;
        }
    }
    const failOrPass = numberOfTestsPassed === numberOfTestsTotal ? 'PASS' : 'FAIL';
    console.log(`[${failOrPass}] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests.`);

    // Return all grammars
    if (newTested) {
        return [...TestedPrevious, newTested];
    } else {
        return TestedPrevious;
    }
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

async function repairGrammar(openaiEnv: OpenAIEnv, messages: OpenAIMessage[], testedSnippets: TestedSnippet[], snippet: Snippet, previousSnippets: Snippet[]): Promise<TestedSnippet[]> {
    console.log(messages);
    throw new Error("Not implemented");
    const maxRetries = 8;
    
    let currentGrammar: Grammar = { ...testedSnippets[0].usedGrammar };
    let currentTestedSnippets = testedSnippets;
    for (let i = 0; i < maxRetries; i++) {
        const prompt = `fix this shit`;

        // TODO: Get repaired lexer and parser from llm
        
        // TODO: Check if there are any new errors

        // TODO: If there are new errors, update currentTestedSnippets
    }

    return currentTestedSnippets;
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
    snippets: Snippet[] = []
): Promise<[OpenAIMessage[], Grammar]> {
    let g: Grammar = {
        lexerSource: 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE\n',
        parserSource: 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
    };
    if (initalLexer) g.lexerSource = initalLexer;
    if (initalParser) g.parserSource = initalParser;
    const includeErrors = !!(initalLexer && initalParser);

    let firstNonWorkingTestedSnippet: TestedSnippet = {
        onSnippet: snippets[0],
        usedGrammar: g,
        errors: undefined,
        success: false,
    };
    if (initalLexer && initalParser) {
        firstNonWorkingTestedSnippet = (await testGrammarOnMany(g, undefined, snippets, true))[0];
        if (firstNonWorkingTestedSnippet.success) throw new Error("First non working snippet should fail!");
    }

    console.log("Generating initial guess for the first intermediate solution...");
    const [messages, initialGrammar] = await generateInitalGuess(openaiEnv, firstNonWorkingTestedSnippet, snippets, g.lexerSource, g.parserSource, includeErrors);

    return [messages, initialGrammar];
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


async function createQualifiedCandiate(grammar: Grammar, allSnippets: Snippet[]): Promise<Candidate> {
    if (allSnippets.length === 0) {
        throw new Error('No tested snippets given.');
    }
    const testedSnippets = await testGrammarOnMany(grammar, undefined, allSnippets);
    const candidate: Candidate = {
        grammar: grammar,
        snippets: testedSnippets,
        score: testedSnippets.filter(testedSnippet => testedSnippet.success).length,
    };
    return candidate;
}

export async function doInferGrammar(directory: string, extension: string, outputDir: string, options: CLIInferGrammarArguments) {
    // const maxRetries = 3;

    const sortedSnippets = await loadSnippetsByComplexity(directory, extension, options.recursive);
    if (sortedSnippets === undefined || sortedSnippets.length === 0) {
        console.error('No files found with the given extension.');
        return;
    }

    // Create temporary lexer and parser files
    const partialLexerFilePath = createTemporaryFile(outputDir, 'MyLexer.partial.g4');
    const partialParserFilePath = createTemporaryFile(outputDir, 'MyParser.partial.g4');

    // Load OpenAI environment variables
    const openaiEnv = loadOpenAIEnvVars();

    // Load initial lexer and parser if they exist
    const initialLexer = loadFile(options.initialLexer);
    const initialParser = loadFile(options.initialParser);

    // If both initial lexer and parser are provided, let's just check if they already are syntactically valid
    if (initialLexer && initialParser && options.initialLexer && options.initialParser) {
        const errors = await checkGrammarOnMany(options.initialLexer, options.initialParser, sortedSnippets.map(snippet => snippet.filePath));
        if (errors.length === 0) {
            console.log("Loaded grammar (lexer and parser) is syntactically valid! Exiting...");
            process.exit(0);
        } else {
            console.error("Loaded grammar (lexer and parser) are NOT syntactically valid!");
        }
    }


    // Build our first intermediate solution.
    // Static initialization (controlled by `options.skipFirstGuess`):
    //      1. Empty grammar (just a template)
    //      2. The initial lexer and parser
    // Dynamic initialization:
    //      1. Guess based on all the snippets
    //      2. Guess based on the snippets and the initial lexer and parser
    const snippetsUsedInGuess = sortedSnippets; // TODO:   shouldn't be all the files when building first candidate
                                                // TODO... maybe we can do tokens similarity matching to find differing samples. Although we should test on all later.
    let [messages, currentIntermediateSolution] = await buildFirstIntermediateSolution(openaiEnv,
        initialLexer,
        initialParser,
        snippetsUsedInGuess
    );

    const snippetHistory: Snippet[] = [];
    const candiateHistory: Candidate[] = [];

    for (const snippet of sortedSnippets) {
        // Write grammar to temporary files (lexer and parser)
        fs.writeFileSync(partialLexerFilePath, currentIntermediateSolution.lexerSource);
        fs.writeFileSync(partialParserFilePath, currentIntermediateSolution.parserSource);

        // Test current solution against this snippet and history
        const testedSnippets = await testGrammarOnMany(currentIntermediateSolution, snippet, snippetHistory);
        const completeSuccess = testedSnippets.every(testedSnippet => testedSnippet.success);

        candiateHistory.push(await createQualifiedCandiate(currentIntermediateSolution, sortedSnippets));

        if (!completeSuccess) {
            console.log("Current solution failed some tests, attempting repair...");
            // Try to repair the grammar
            const repairedTestedSnippets = await repairGrammar(openaiEnv, messages, testedSnippets, snippet, snippetHistory);
            if (repairedTestedSnippets.length === 0) {
                throw new Error("What happened to all the repaired snippets?");
            }
            currentIntermediateSolution = repairedTestedSnippets[0].usedGrammar;
        }

        snippetHistory.push(snippet);
        console.log(`[New intermediate solution] Inferred grammar from ${snippet.fileName} (complexity=${calculateComplexity(snippet.snippet)})`);
    }

    // Find the best scored candidate
    const bestCandidate = candiateHistory.sort((a, b) => b.score - a.score)[0];
    if (bestCandidate.snippets.length === 0) {
        console.error("Failed to find ANY valid grammar!");
        ExitAndLogStats(1);
        return;
    }
    console.log("A final solution found with complexity", calculateComplexity(bestCandidate.grammar.lexerSource + bestCandidate.grammar.parserSource));
    if (bestCandidate.score !== bestCandidate.snippets.length) {
        console.log("NOTE: this isn't a perfect solution, it only covers", bestCandidate.score, "of", bestCandidate.snippets.length, "snippets.");
    }

    // Write to current directory
    const outputLexerFilePath = path.join(outputDir, 'MyLexer.g4');
    const outputParserFilePath = path.join(outputDir, 'MyParser.g4');
    fs.writeFileSync(outputLexerFilePath, bestCandidate.grammar.lexerSource);
    fs.writeFileSync(outputParserFilePath, bestCandidate.grammar.parserSource);
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