import fs from 'fs';
import { callSWIProlog } from '../semantic/call';
import type { CLIGenerateArguments } from '../cli';
import { compileANTLRFiles } from '../syntactic/build';
import { _createWorkingDirectory, _createTemporaryFile } from './utils/io';
import { checkGrammar } from '../syntactic/check-grammar';
import path from 'path';

interface PrologQueryData {
    filePath: string;
    clauses: string[];
}

interface QueryResult {
    ast: string | null;
    errors: string[];
}

// Core function to generate Prolog query
export async function generatePrologQuery(
    targetPath: string,
    lexerPath: string,
    parserPath: string,
    adapterPath: string | undefined,
    queryPath: string,
): Promise<PrologQueryData> {
    const workingDirectory = _createWorkingDirectory('query');

    // Check grammar validity
    const antlrJavaErrors = await checkGrammar(lexerPath, parserPath, targetPath);
    if (antlrJavaErrors.length > 0) {
        throw new Error('Grammar validation failed: ' + antlrJavaErrors.map(error => error.message).join('\n'));
    }

    // Setup grammar files
    const grammarPaths = {
        lexer: path.join(workingDirectory, 'MyLexer.g4'),
        parser: path.join(workingDirectory, 'MyParser.g4'),
    };

    fs.copyFileSync(lexerPath, grammarPaths.lexer);
    fs.copyFileSync(parserPath, grammarPaths.parser);

    // Compile ANTLR
    const antlrCompilationErrors = await compileANTLRFiles(workingDirectory);
    if (antlrCompilationErrors.length > 0) {
        throw new Error('ANTLR compilation failed: ' + antlrCompilationErrors.join('\n'));
    }

    // Parse target
    const { createParserFromGrammar } = await import('../syntactic/context-free-parser');
    const { clauseGenerator: cstToAstGeneratorClauses, queryClauses } = await import('../semantic/prolog');

    const targetContent = fs.readFileSync(targetPath, 'utf8');
    const { parser, errorListener } = await createParserFromGrammar(
        targetContent,
        grammarPaths.lexer,
        grammarPaths.parser
    );

    const tree = parser.program();
    if (errorListener.hasErrors()) {
        throw new Error('Parsing failed: ' + errorListener.getErrors().join('\n'));
    }

    // Generate clauses
    const clauses = [
        ...cstToAstGeneratorClauses(tree, parser, adapterPath),
        ...queryClauses(queryPath)
    ];

    if (clauses.length === 0) {
        throw new Error('No clauses generated');
    }

    const outputPath = path.join(workingDirectory, 'query.pl');
    fs.writeFileSync(outputPath, clauses.join('\n'));

    return {
        filePath: outputPath,
        clauses
    };
}

// Execute Prolog query
export function executePrologQuery(prologFile: string): QueryResult {
    const result = callSWIProlog(prologFile);
    return {
        ast: result.stdout.trim() || null,
        errors: result.stderr ? [result.stderr] : []
    };
}

// Main function - maintains original CLI behavior
export async function doQuery(
    targetPath: string,
    lexerPath: string,
    parserPath: string,
    options: CLIGenerateArguments
): Promise<void> {
    try {
        // Generate Prolog query
        const prologData = await generatePrologQuery(targetPath, lexerPath, parserPath, options.adapter, options.query);

        console.log("Target:", targetPath);
        console.log("Target parsed successfully.");

        // Handle no-adapter case
        if (options.adapter === undefined) {
            console.warn("WARN: Since no adapter was provided, the query will not be run.");
            console.log("Generated prolog file:", prologData.filePath);
            process.exit(1);
            return;
        }

        // Execute query
        console.log("Running Prolog.");
        const queryResult = executePrologQuery(prologData.filePath);

        // Handle results
        if (queryResult.errors.length > 0) {
            console.error("Prolog failed:");
            queryResult.errors.forEach(error => console.error(error));
        }

        if (!queryResult.ast) {
            console.error("Prolog returned empty result");
        } else {
            console.log(queryResult.ast);
        }

    } catch (error) {
        if (error instanceof Error) {
            console.error(error.message);
        } else {
            console.error('An unexpected error occurred:', error);
        }
        process.exit(1);
    }
}