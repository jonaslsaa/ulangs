import fs from 'fs';
import { callSWIProlog } from '../semantic/call';
import type { CLIQueryArguments } from '../cli';
import { compileANTLRFiles } from '../syntactic/build';
import { _createWorkingDirectory, _createTemporaryFile } from './utils/io';
import { checkGrammar } from '../syntactic/check-grammar';
import path from 'path';
import { GetQuery, type Query } from '../rules/queries/mapping';

interface PrologQueryData {
    filePath: string;
    clauses: string[];
}

interface QueryResult {
    output: string | null;
    errors: string[];
}

// Core function to generate Prolog query
export async function generatePrologQuery(
    targetPath: string,
    lexerPath: string,
    parserPath: string,
    adapterPath: string | undefined,
    query: Query,
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
    const { getApplicableTreeClauses, getQueryClauses } = await import('../semantic/prolog');

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
        ...getApplicableTreeClauses(tree, parser, adapterPath),
        ...getQueryClauses(query.path)
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
        output: result.stdout.trim() || null,
        errors: result.stderr ? [result.stderr] : []
    };
}

// Main function - maintains original CLI behavior
export async function doQuery(
    targetPath: string,
    lexerPath: string,
    parserPath: string,
    options: CLIQueryArguments
): Promise<void> {
    try {
        
        const query = GetQuery(options.query);

        // Generate Prolog query
        const prologData = await generatePrologQuery(targetPath, lexerPath, parserPath, options.adapter, query);

        console.log("Target:", targetPath);

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

        if (!queryResult.output) {
            console.error("Prolog returned empty result");
        } else {
            // Validate output against the schema
            if (!options.ignoreSchema) {
                const schema = query.schema.safeParse(JSON.parse(queryResult.output));
                if (!schema.success) {
                    console.error("Prolog returned invalid JSON");
                    console.error(schema.error.message);
                }
            }
            console.log(queryResult.output);
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