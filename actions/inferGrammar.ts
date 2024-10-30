import path from "path";
import type { CLIInferGrammarArguments } from "../cli";
import fs from 'fs';
import { calculateComplexity } from "../heuristics/complexity";
import { loadOpenAIEnvVars, type OpenAIEnv } from "../llm/utils";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { generateCandidateSolutions, generateInitalGuess, repairCandidateSolution, Stats, type GrammarWithMessageHistory, type Grammar, type Snippet } from '../llm/grammar';
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

type TestedGrammar = {
    grammarWithHistory: GrammarWithMessageHistory;
    errors?: ANTLRError[];
    success: boolean;
    complexity: number;
};

async function testGrammar(grammarWithHistory: GrammarWithMessageHistory, snippet: Snippet): Promise<TestedGrammar> {
    if (grammarWithHistory.grammar === undefined) {
        throw new Error("Grammar is undefined");
    }
    // try to build the grammar 
    // by creating temporary directory
    // write grammars to file
    // run antlr4 to build the grammar (.ts files)

    // try to parse the snippet to a tree
    // by importing newly compiled .ts file
    // trying to parse the snippet to a tree
    // check if errorlistener has errors
    // return result
    const grammar = grammarWithHistory.grammar;
    const testedGrammar: TestedGrammar = {
        grammarWithHistory: grammarWithHistory,
        success: false,
        complexity: calculateComplexity(`${grammar.lexerSource}\n${grammar.parserSource}`)
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

async function testGrammarOnMany(grammarWithHistory: GrammarWithMessageHistory,
                                    newSnippet: Snippet,
                                    previousSnippets: Snippet[]): Promise<TestedGrammar> {
    let numberOfTestsPassed = 0;
    let numberOfTestsTotal = 1 + previousSnippets.length;
    // Test first on new snippets
    const TestedGrammarMain = await testGrammar(grammarWithHistory, newSnippet);
    if (!TestedGrammarMain.success) {
        console.log(`[FAILED] Passed ${numberOfTestsPassed}/${numberOfTestsTotal} tests. Failed on ${newSnippet.fileName}!`);
        return TestedGrammarMain;
    }
    numberOfTestsPassed++;
    // Now test all previous snippets
    const TestedGrammarPrevious = await Promise.all(previousSnippets.map(async previousSnippet => {
        const TestedGrammarPrevious = await testGrammar(grammarWithHistory, previousSnippet);
        // TODO: do something with the history/messages?´
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


async function repairGrammars(openaiEnv: OpenAIEnv, testedGrammars: TestedGrammar[], snippet: Snippet, previousSnippets: Snippet[]): Promise<TestedGrammar | undefined> {
    console.log("Attempting to repair invalid grammars based on errors...");
    const maxRetries = 6;
    const validRepairedGrammars: TestedGrammar[] = [];
    for (let i = 0; i < maxRetries; i++) {
        if (i > 0) console.error(` * Repair attempt ${i + 1}/${maxRetries}...`);
        // Repair all candidate grammars
        const repairedCandidateGrammars = await Promise.all(testedGrammars.map(async testedGrammar => {
            const newErrors: ANTLRError[] = [...(testedGrammar.errors ?? [])]
            // TODO: check that correct errors are being passed, should we filter out errors that are not in the snippet?
            const r = await repairCandidateSolution(openaiEnv, testedGrammar.grammarWithHistory, newErrors);
            testedGrammar.grammarWithHistory.messages = r.messages;
            return r;
        }));

        // Filter out undefined grammars
        const repairedCandidateGrammarsFiltered = repairedCandidateGrammars.filter(candidateGrammar =>
            candidateGrammar.grammar !== undefined
        );

        // Test each repaired candidate grammar
        const repairedTestedGrammars = await Promise.all(repairedCandidateGrammarsFiltered.map(async candidateGrammar => {
            const grammar = candidateGrammar.grammar;
            if (grammar === undefined) {
                throw new Error('Grammar is undefined');
            }
            const g: GrammarWithMessageHistory = {
                grammar: grammar,
                messages: candidateGrammar.messages
            }
            return await testGrammarOnMany(g, snippet, previousSnippets);
        }));

        // Filter out invalid grammars
        validRepairedGrammars.push(...repairedTestedGrammars.filter(g => g.success));
        console.log(`Generated ${repairedTestedGrammars.length} candidate repairs - ${validRepairedGrammars.length} succeeded`);

        // Add the score of the repaired grammars to the overall score
        validRepairedGrammars.forEach(g => Stats.addScore(g.grammarWithHistory.grammar.generatedWithModel));

        if (validRepairedGrammars.length > 0) {
            break;
        }
    }

    if (validRepairedGrammars.length === 0) {
        return undefined;
    }

    // Select the best repaired grammar based on complexity
    let bestRepairedGrammar = validRepairedGrammars[0];
    if (validRepairedGrammars.length > 1) {
        console.log("Selecting the best repaired grammar...");
        for (const grammar of validRepairedGrammars) {
            if (grammar.complexity < bestRepairedGrammar.complexity) {
                bestRepairedGrammar = grammar;
            }
        }
    }

    return bestRepairedGrammar;
}

async function generateNextIntermediateSolution(
    openaiEnv: OpenAIEnv,
    currentIntermediateSolution: Grammar,
    snippet: Snippet,
    previousSnippets: Snippet[]
): Promise<TestedGrammar | undefined> {
    console.log(`Inferring grammar from ${snippet.fileName} (complexity=${calculateComplexity(snippet.snippet)})`);

    // First see if the current intermediate solution is valid
    const errors: ANTLRError[] = [];
    if (currentIntermediateSolution) {
        const testedGrammar = await testGrammarOnMany({ grammar: currentIntermediateSolution, messages: [] }, snippet, previousSnippets);
        if (!testedGrammar.success) {
            console.log("Current intermediate solution is invalid, trying to generate a new one.");
            if (testedGrammar.errors === undefined || testedGrammar.errors.length === 0) {
                throw new Error('No errors found in the grammar, but it failed?');
            }
            errors.push(...testedGrammar.errors);
        } else {
            console.log("Current intermediate solution is valid.");
            return testedGrammar;
        }
    }

    // Generate candidate grammars from LLM
    const N = 20;
    const candidateGrammars = await generateCandidateSolutions(openaiEnv,
        currentIntermediateSolution,
        snippet.snippet,
        errors,
        N
    );
    console.log(`Generated ${candidateGrammars.length}/${N} candidate grammars`);

    // Test each candidate grammar
    const testedGrammars = await Promise.all(candidateGrammars.map(async candidateGrammar => {
        return await testGrammarOnMany(candidateGrammar, snippet, previousSnippets);
    }));

    // Filter out invalid grammars
    const validGrammars = testedGrammars.filter(g => g.success);
    console.log(`Tested ${testedGrammars.length} candidate grammars - ${validGrammars.length} succeeded`);

    // If no valid grammars were found, try to repair them
    if (validGrammars.length === 0) {
        return await repairGrammars(openaiEnv, testedGrammars, snippet, previousSnippets);
    }

    // Give models points for correct grammars
    validGrammars.forEach(g => Stats.addScore(g.grammarWithHistory.grammar.generatedWithModel));

    // Select the best grammar based on complexity
    let bestGrammar = validGrammars[0];
    for (const grammar of validGrammars) {
        if (grammar.complexity < bestGrammar.complexity) {
            bestGrammar = grammar;
        }
    }
    if (validGrammars.length > 1) {
        console.log(`Selected best grammar with complexity ${bestGrammar.complexity}`);
    }
    return bestGrammar;
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
        const newG = (await generateInitalGuess(openaiEnv, snippets, g.lexerSource, g.parserSource, fileNamesThatDidntPass)).grammar;
        if (newG === undefined) {
            throw new Error('Failed to generate initial guess');
        }
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

export async function doInferGrammar(directory: string, extension: string, outputDir: string, options: CLIInferGrammarArguments) {
    const maxRetries = 3;

    const files = findAllCodeFiles(directory, extension, options.recursive);
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

    // Create temporary lexer and parser files
    const tempLexerFilePath = createTemporaryFile(outputDir, 'MyLexer.tmp.g4');
    const tempParserFilePath = createTemporaryFile(outputDir, 'MyParser.tmp.g4');

    // Load OpenAI environment variables
    const openaiEnv = loadOpenAIEnvVars();

    // Load initial lexer and parser if they exist
    const initialLexer = loadFile(options.initialLexer);
    const initialParser = loadFile(options.initialParser);

    // If both initial lexer and parser are provided, let's just check if they already are syntactically valid
    let fileNamesThatDidntPass: string[] = [];
    if (initialLexer && initialParser && options.initialLexer && options.initialParser) {
        const errors = await checkGrammarOnMany(options.initialLexer, options.initialParser, sortedSnippets.map(snippet => snippet.filePath));
        if (errors.length === 0) {
            console.log("Loaded grammar (lexer and parser) is syntactically valid! Exiting...");
            process.exit(0);
        } else {
            console.error("Loaded grammar (lexer and parser) are NOT syntactically valid!");
        }
        errors.forEach(error => error.file && fileNamesThatDidntPass.push(error.file));
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
                                            fileNamesThatDidntPass
                                        );
    

    const snippetHistory: Snippet[] = [];
    let didWeGiveUp = false;
    let lastSnippetBeforeGiveUp: Snippet | undefined = undefined;

    for (const snippet of sortedSnippets) {
        // Write snippet to temporary files
        fs.writeFileSync(tempLexerFilePath, currentIntermediateSolution.lexerSource);
        fs.writeFileSync(tempParserFilePath, currentIntermediateSolution.parserSource);

        let nextIntermediateSolution: TestedGrammar | undefined = undefined;

        for (let i = 0; i < maxRetries; i++) {
            if (i > 0) {
                console.error(` * Retry attempt ${i + 1}/${maxRetries}...`);
            }

            nextIntermediateSolution = await generateNextIntermediateSolution(
                openaiEnv,
                currentIntermediateSolution,
                snippet,
                snippetHistory
            );
            if (nextIntermediateSolution) {
                break;
            }
        }

        if (nextIntermediateSolution) {
            currentIntermediateSolution = nextIntermediateSolution.grammarWithHistory.grammar;
        } else {
            didWeGiveUp = true;
            lastSnippetBeforeGiveUp = snippet;
            break;
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