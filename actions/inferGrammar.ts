import path from "path";
import type { CLIInferGrammarArguments } from "../cli";
import fs from 'fs';
import { calculateComplexity } from "../heuristics/complexity";
import { loadOpenAIEnvVars, type OpenAIEnv, type OpenAIMessage } from "../llm/utils";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { constructPrompt, generateInitalGuess, makeCompletionRequest, Stats, type Grammar, type Snippet, type TestedSnippet } from '../llm/grammar';
import { checkGrammar } from "../syntactic/check-grammar";
import { _createWorkingDirectory, _createTemporaryFile, findAllCodeFiles, loadFile } from './utils/io';
import { compressMessages } from '../llm/compress-messages';
import { loadSnippetsByComplexity } from './utils/snippets';

const temporaryFileDirectoryRecords = new Set<string>();

const createWorkingDirectory = () => _createWorkingDirectory('grammar');

const createTemporaryFile = (dir: string, fileName: string) => _createTemporaryFile(dir, fileName, temporaryFileDirectoryRecords);

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

    const tempPath = createWorkingDirectory();

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
    let numberOfTestsTotal = previousSnippets.length;

    let newTested: TestedSnippet | undefined = undefined;
    if (newSnippet) {
        numberOfTestsTotal += 1; // add one for the new snippet if defined
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
            if (!tested.success) return [tested];
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

export async function repairGrammar(
    openaiEnv: OpenAIEnv,
    messages: OpenAIMessage[],  // prior LLM messages or relevant conversation context
    testedSnippets: TestedSnippet[],       // result(s) after the grammar failed on the `snippet`
    snippet: Snippet,                      // the snippet that triggered the “need repair” scenario
    previousSnippets: Snippet[]            // the ones that have passed so far (and must remain passing)
): Promise<TestedSnippet[]> {
    /**
     * This function tries to fix the grammar to parse `snippet` successfully without
     * breaking previous snippets that were already passing. It uses GPT (or another OpenAI model)
     * in a loop, up to `maxRetries` times, asking it to “repair” the grammar. Each iteration:
     *   1. Identifies the first failing snippet (usually `snippet` if `stopOnFirstFailure=true`).
     *   2. Asks the LLM for a repaired grammar (by constructing a “repair mode” prompt).
     *   3. Tries the new grammar on the snippet (and re-checks all previously passing snippets).
     *   4. If it still fails, tries again until success or max attempts.
     */
    const maxRetries = 5;

    // Start from the grammar used in the failing test. Typically `testedSnippets[0]` is the new snippet test result
    // if `stopOnFirstFailure` was used. But if multiple snippets are tested simultaneously,
    // you might want to find the first one that failed in the array:
    const firstFailIndex = testedSnippets.findIndex(ts => !ts.success);
    if (firstFailIndex < 0) {
        // If everything is already passing, just return what we got:
        return testedSnippets;
    }

    let currentGrammar: Grammar = { ...testedSnippets[firstFailIndex].usedGrammar };
    let currentTestedSnippets = testedSnippets;

    // Attempt to fix the grammar up to maxRetries times
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
        console.log(`\n[Repair Attempt ${attempt}/${maxRetries}] Trying to fix grammar for snippet: ${snippet.fileName}`);

        // We build a “repair prompt” that includes the errors from the first failing snippet
        // (or whichever snippet is failing). We set `repairMode = true` and `includeErrors = true`.
        // This prompt helps GPT see the current grammar, the snippet, and the error messages
        // overlaid on the appropriate lines.

        const firstFailTest = currentTestedSnippets.find(ts => !ts.success);
        if (!firstFailTest) {
            // No snippet is failing => everything is good, break out early
            console.log(`All snippets are now passing; no need for further repairs.`);
            break;
        }

        // Construct a “repair mode” prompt focusing on the first failing snippet.
        constructPrompt(
            currentGrammar,
            firstFailTest,               // the failing TestedSnippet
            undefined,                   // we pass undefined as the snippets are already included in the conversation history
            messages,                    // we pass the prior conversation history, this will be updated within this function
        /* repairMode */ true,
        /* includeErrors */ true,
        /* appendToMessage */ `\n(Repair attempt #${attempt}, please correct any mistakes so the code snippet parses successfully without breaking previously passing snippets.)`
        );

        // Now we ask the LLM for a repaired grammar
        console.log(messages);
        const completionResult = await makeCompletionRequest(
            openaiEnv,
            messages,
            openaiEnv.model,
        /* debug */ undefined,      // set to 'input'|'output'|'both' if you want logs
        /* timeoutSeconds */ 60 * 5
        );

        if (completionResult.isErr()) {
            // Something went wrong with the LLM request (e.g., API error or timeout)
            console.error(`[Repair Attempt ${attempt}] LLM request failed: ${completionResult.error}`);
            // Decide if you want to continue or break. Here we just break:
            break;
        }

        // If we got a successful completion, parse the returned grammar
        const newGrammar = completionResult.unwrap();

        // Update currentGrammar
        currentGrammar = {
            ...newGrammar,
            generatedWithModel: openaiEnv.model,  // e.g., track which model generated it
        };

        // Now test the new grammar on (1) the new snippet and (2) the previously passing snippets
        // If we do “stopOnFirstFailure: true” we only get the first failing snippet. Otherwise we get them all.
        // Here we do “stopOnFirstFailure: true” so we short-circuit if the snippet fails again.
        currentTestedSnippets = await testGrammarOnMany(
            currentGrammar,
            snippet,
            previousSnippets,
        /* stopOnFirstFailure */ true
        );

        // If the new snippet is now passing, re-check that none of the previously passing snippets
        // are broken. (If you already used testGrammarOnMany with all snippets at once, you can skip this part.)
        const stillFailing = currentTestedSnippets.filter(ts => !ts.success);
        if (stillFailing.length === 0) {
            console.log(`[Repair Attempt ${attempt}] ✅ Grammar repaired successfully for snippet: ${snippet.fileName}`);
            return currentTestedSnippets;
        } else {
            // The snippet still fails or we broke something else. Try again.
            console.log(`[Repair Attempt ${attempt}] ❌ Still failing. Will attempt another repair.`);
        }
    }

    // If we reach here, we exhausted the allowed attempts without fully fixing the grammar.
    console.warn(`\n[Repair] Max attempts (${maxRetries}) reached; returning last tested result.`);
    return currentTestedSnippets;
}

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

async function buildFirstIntermediateSolution(openaiEnv: OpenAIEnv,
    initalLexer: string | undefined,
    initalParser: string | undefined,
    messages: OpenAIMessage[],
    snippets: Snippet[] = []
): Promise<Grammar> {
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
    return await generateInitalGuess(openaiEnv, firstNonWorkingTestedSnippet, snippets, messages, g.lexerSource, g.parserSource, includeErrors);
}

async function checkGrammarOnMany(lexerPath: string, parserPath: string, codePaths: string[]): Promise<ANTLRError[]> {
    const results = await Promise.all(codePaths.map(codePath => checkGrammar(lexerPath, parserPath, codePath)));
    return results.flat();
}

async function createQualifiedCandiate(grammar: Grammar, allSnippets: Snippet[]): Promise<Candidate> {
    if (allSnippets.length === 0) {
        throw new Error('No tested snippets given.');
    }
    console.log(`Saving a candidate, with following score:`);
    const testedSnippets = await testGrammarOnMany(grammar, undefined, allSnippets);
    const candidate: Candidate = {
        grammar: grammar,
        snippets: testedSnippets,
        score: testedSnippets.filter(testedSnippet => testedSnippet.success).length,
    };
    return candidate;
}


export async function doInferGrammar(directory: string, extension: string, outputDir: string, options: CLIInferGrammarArguments) {

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

    let messages: OpenAIMessage[] = [];

    // Build our first intermediate solution.
    // Static initialization (controlled by `options.skipFirstGuess`):
    //      1. Empty grammar (just a template)
    //      2. The initial lexer and parser
    // Dynamic initialization:
    //      1. Guess based on all the snippets
    //      2. Guess based on the snippets and the initial lexer and parser
    const snippetsUsedInGuess = sortedSnippets; // TODO:   shouldn't be all the files when building first candidate
    // TODO... maybe we can do tokens similarity matching to find differing samples. Although we should test on all later.
    let currentIntermediateSolution = await buildFirstIntermediateSolution(openaiEnv,
        initialLexer,
        initialParser,
        messages,
        snippetsUsedInGuess
    );

    const snippetHistory: Snippet[] = [];
    const candiateHistory: Candidate[] = [];

    candiateHistory.push(await createQualifiedCandiate(currentIntermediateSolution, sortedSnippets));

    for (const snippet of sortedSnippets) {
        // Write grammar to temporary files (lexer and parser)
        fs.writeFileSync(partialLexerFilePath, currentIntermediateSolution.lexerSource);
        fs.writeFileSync(partialParserFilePath, currentIntermediateSolution.parserSource);

        // Test current solution against this snippet and history
        const testedSnippets = await testGrammarOnMany(currentIntermediateSolution, snippet, snippetHistory);
        const completeSuccess = testedSnippets.every(testedSnippet => testedSnippet.success);

        if (!completeSuccess) {
            console.log("Current solution failed some tests, attempting repair...");
            // Try to repair the grammar
            const repairedTestedSnippets = await repairGrammar(openaiEnv, messages, testedSnippets, snippet, snippetHistory);
            if (repairedTestedSnippets.length === 0) {
                throw new Error("What happened to all the repaired snippets?");
            }
            currentIntermediateSolution = repairedTestedSnippets[0].usedGrammar;
            candiateHistory.push(await createQualifiedCandiate(currentIntermediateSolution, sortedSnippets));

            // Compress the messages
            messages = compressMessages(messages);
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