import path from "path";
import type { CLIInferGrammarArguments } from "../cli";
import fs from 'fs';
import { calculateComplexity } from "../heuristics/complexity";
import { loadOpenAIEnvVars, type OpenAIEnv } from "../llm/utils";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { generateCandidateSolutions, type Grammar } from "../llm/grammar";
import { compileANTLRFiles } from "../syntactic/build";
import { createParserFromGrammar } from "../syntactic/context-free-parser";

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

type Snippet = {
    snippet: string;
    fileName: string;
}

function loadFilesToSnippets(files: string[]): Snippet[] {
    return files.map(file => {
        const fileContent = fs.readFileSync(file, 'utf8');
        return {
            snippet: fileContent,
            fileName: path.basename(file)
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
    complexity: number;
};

async function testGrammar(grammar: Grammar, snippet: string): Promise<TestedGrammar> {
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
        complexity: calculateComplexity(`${grammar.lexerSource}\n${grammar.parserSource}`)
    };

    const timestamp = new Date().toISOString().replaceAll(':', '-');
    const tempPath = createTemporaryDirectory(timestamp);

    const lexerFilePath = path.join(tempPath, 'MyLexer.g4');
    const parserFilePath = path.join(tempPath, 'MyParser.g4');

    fs.writeFileSync(lexerFilePath, grammar.lexerSource);
    fs.writeFileSync(parserFilePath, grammar.parserSource);

    const errors = compileANTLRFiles(tempPath);
    if (errors.length > 0) {
        testedGrammar.errors = errors;
        return testedGrammar;
    }

    // Try to parse the snippet
    try {
        const { parser, errorListener } = await createParserFromGrammar(snippet, lexerFilePath, parserFilePath);
        parser.program();
        if (errorListener.hasErrors()) {
            testedGrammar.errors = errorListener.getErrors();
            return testedGrammar;
        }
    } catch (error: any) {
        console.error(error);
        process.exit(1);
        testedGrammar.errors = [{
            isWarning: false,
            grammarType: 'UNKNOWN',
            source: 'BUILD',
            message: error.message,
            file: lexerFilePath,
            line: undefined,
            column: undefined,
        }];
        return testedGrammar;
    }

    console.log(`Successfully tested grammar at path:\n- ${lexerFilePath}\n- ${parserFilePath}`);
    testedGrammar.success = true;
    return testedGrammar;
}

async function generateNextIntermediateSolution(openaiEnv: OpenAIEnv, currentIntermediateSolution: Grammar | undefined, snippet: Snippet, previousSnippets: Snippet[]): Promise<TestedGrammar | undefined> {
    console.log(`Inferring grammar from ${snippet.fileName} (complexity=${calculateComplexity(snippet.snippet)})`);

    // First see if the current intermediate solution is valid
    const errors: string[] = [];
    let errorUnderCompilationOfANTLRFiles = false;
    if (currentIntermediateSolution) {
        const testedGrammar = await testGrammar(currentIntermediateSolution, snippet.snippet);
        // TODO: Test all previous snippets if the current one succeedes, an intermediate must be valid for all previous snippets
        if (!testedGrammar.success) {
            errors.push(...testedGrammar.errors?.map(error => error.message) ?? []);
            errorUnderCompilationOfANTLRFiles = testedGrammar.errors?.some(error => error.source === 'BUILD') ?? false;
            console.log("Current intermediate solution is invalid, trying to generate a new one.");
        } else {
            console.log("Current intermediate solution is valid.");
            return testedGrammar;
        }
    }

    // Generate candidate grammars from LLM
    const candidateGrammars = await generateCandidateSolutions(openaiEnv,
                                                                currentIntermediateSolution,
                                                                snippet.snippet,
                                                                errors,
                                                                errorUnderCompilationOfANTLRFiles
                                                            );
    console.log(`Generated ${candidateGrammars.length} candidate grammars`);
    // Test each candidate grammar
    const testedGrammars = await Promise.all(candidateGrammars.map(async candidateGrammar => {
        const testedGrammar = await testGrammar(candidateGrammar, snippet.snippet);
        // TODO: Test all previous snippets if the current one succeedes, an intermediate must be valid for all previous snippets
        return testedGrammar;
    }));
    // Filter out invalid grammars
    const validGrammars = testedGrammars.filter(g => g.success);
    console.log(`Tested ${testedGrammars.length} candidate grammars - ${validGrammars.length} succeeded`);

    // If no valid grammars were found, return undefined
    if (validGrammars.length === 0) {
        // Log the errors for debugging
        if (testedGrammars.length > 0) {
            console.log("Errors for the first grammar:");
            testedGrammars[0].errors?.forEach(error => console.error(error));
        }
        return undefined;
    }

    // If multiple valid grammars were found, select the best one by heuristic
    let bestGrammar: TestedGrammar = validGrammars[0];
    for (const grammar of validGrammars) {
        if (grammar.complexity < bestGrammar.complexity) {
            bestGrammar = grammar;
        }
    }
    return bestGrammar;
}

export async function doInferGrammar(directory: string, extension: string, options: CLIInferGrammarArguments) {
    const maxRetries = 3;

    const files = findAllCodeFiles(directory, extension, options.recursive);
    const snippets = loadFilesToSnippets(files.map(file => path.join(directory, file)));
    const sortedSnippets = sortByComplexity(snippets);

    const lowestComplexity = sortedSnippets[0];
    const highestComplexity = sortedSnippets[sortedSnippets.length - 1];
    console.log(`Loaded ${files.length} files and sorted them by complexity: ${calculateComplexity(lowestComplexity.snippet)} to ${calculateComplexity(highestComplexity.snippet)}`);

    // Load OpenAI environment variables
    const openaiEnv = loadOpenAIEnvVars();

    // For each snippet, try to find a intermediate solution
    // By generating candidate solutions and testing them, if they are valid they become the new intermediate solution
    let currentIntermediateSolution: Grammar | undefined = undefined;
    const snippetHistory: Snippet[] = [];
    let didWeGiveUp = false;
    for (const snippet of sortedSnippets) {
        let nextIntermediateSolution = undefined;
        for (let i = 0; i < maxRetries; i++) {
            nextIntermediateSolution = await generateNextIntermediateSolution(openaiEnv, currentIntermediateSolution, snippet, snippetHistory);
            if (nextIntermediateSolution) {
                break;
            }
            console.error(" * Retrying...");
        }

        // If no valid intermediate solution was found, break
        if (nextIntermediateSolution) {
            currentIntermediateSolution = nextIntermediateSolution.grammar;
        } else {
            didWeGiveUp = true;
            break;
        }

        snippetHistory.push(snippet);
        console.log(`[New intermediate solution] Inferred grammar from ${snippet.fileName} (complexity=${calculateComplexity(snippet.snippet)})`);
    }

    const finalGrammar = currentIntermediateSolution;
    if (!finalGrammar) {
        console.error("Failed to find a ANY valid grammar!");
        return;
    }

    if (didWeGiveUp) {
        console.error("Failed to find a final grammar.");
        return;
    } else {
        console.log("A final solution found with complexity", calculateComplexity(finalGrammar.lexerSource + finalGrammar.parserSource));
    }
    
    // Write to current directory
    const outputLexerFilePath = path.join(process.cwd(), 'MyLexer.g4');
    const outputParserFilePath = path.join(process.cwd(), 'MyParser.g4');
    fs.writeFileSync(outputLexerFilePath, finalGrammar.lexerSource);
    fs.writeFileSync(outputParserFilePath, finalGrammar.parserSource);
    console.log("Wrote final grammar to", outputLexerFilePath, "and", outputParserFilePath);
}