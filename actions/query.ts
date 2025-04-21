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

type PrologError = {
    file: string;
    line?: number;
    column?: number;
    type: 'ERROR' | 'Warning';
    message: string;
}

interface QueryResult {
    output: string | null;
    errors: PrologError[];
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
    console.log("Prolog path", outputPath);
    fs.writeFileSync(outputPath, clauses.join('\n'));

    return {
        filePath: outputPath,
        clauses
    };
}

const PrologErrorRegex = /^(ERROR|Warning):\s+([^:]+):(\d+)(?::(\d+))?:(.*)$/;
function parseStdErr(stderr: string): PrologError[] {
    const errors: PrologError[] = [];
    const lines = stderr.split('\n');

    for (const line of lines) {
        const match = line.match(PrologErrorRegex);
        if (match) {
            const type = match[1] as 'ERROR' | 'Warning';
            const file = match[2].trim();
            // If the line number is missing, it remains undefined.
            const lineNumber = match[3] ? parseInt(match[3], 10) : undefined;
            // Similarly, if the column is missing, it will be undefined.
            const column = match[4] ? parseInt(match[4], 10) : undefined;
            const message = match[5].trim();

            errors.push({
                file,
                line: lineNumber,
                column,
                type,
                message
            });
        }
    }

    return errors;
}

// Execute Prolog query
export function executePrologQuery(prologFile: string): QueryResult {
    const result = callSWIProlog(prologFile);
    const errors = parseStdErr(result.stderr);
    if (result.timedOut) {
        console.warn("Prolog query timed out.");
        errors.push({
            type: 'ERROR',
            message: 'Prolog query timed out.',
            file: prologFile,
            line: undefined,
            column: undefined,
        });
    }
    if (result.overflowed) {
        console.warn("Prolog query resulted in buffer overflow.");
        errors.push({
            type: 'ERROR',
            message: 'Prolog query resulted in buffer overflow. This means most likely the JSON exploded in size due to an error in the query.',
            file: prologFile,
            line: undefined,
            column: undefined,
        });
    }
    // Filter out errors with empty messages
    const filteredErrors = errors.filter(error => error.message.trim() !== '');
    return {
        output: result.stdout.trim() || null,
        errors: filteredErrors
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
            queryResult.errors.forEach(error => console.error(error.message));
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