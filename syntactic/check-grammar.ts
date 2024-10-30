import { type ANTLRError } from './ErrorListener';
import fs from 'fs';
import { spawn, spawnSync } from 'child_process';

type ANTLRCheckerOutput = {
    warnings: {
        msg: string;
    }[];
    parser_grammar_errors: {
        type: string;
        line: number;
        pos: number;
        msg: string;
    }[];
    lexer_grammar_errors: {
        type: string;
        line: number;
        pos: number;
        msg: string;
    }[];
    result: {
        rules: string[];
        symbols: (string | null)[];
        tokens: {
            type: number;
            line: number;
            pos: number;
            channel: number;
            start: number;
            stop: number;
        }[];
        tree: {
            ruleidx: number;
            alt: number;
            kids: any[];
        };
        lex_errors: {
            startidx: number;
            erridx: number;
            line: number;
            pos: number;
            msg: string;
        }[] | undefined;
        parse_errors: {
            startidx: number;
            erridx: number;
            line: number;
            pos: number;
            msg: string;
        }[] | undefined;
        number_of_nodes: number;
        profile: {
            colnames: string[];
            data: (string | number)[][];
        };
    } | undefined;
};

export async function checkGrammar(lexerPath: string, parserPath: string, codePath: string) {
    // This projects base path
    const basePath = process.cwd();
    const jarFileRelPath = 'antlr-grammar-checker/target/antlr-grammar-checker-1.0-jar-with-dependencies.jar';
    const jarFilePath = basePath + '/' + jarFileRelPath;
    if (!fs.existsSync(jarFilePath)) {
        console.error('ANTLR grammar checker jar file not found at path:', jarFilePath);
        process.exit(1);
    }

    // Check java is in the path
    const javaPath = spawnSync('which', ['java']).stdout.toString().trim();
    if (javaPath === '') {
        console.error('Java is not in the path');
        process.exit(1);
    }

    // Check if the grammar files exist
    if (!fs.existsSync(lexerPath) || !fs.existsSync(parserPath)) {
        console.error('Lexer and parser files not found at paths:', lexerPath, parserPath);
        process.exit(1);
    }

    // Run the grammar checker
    const checkerProcess = spawn('java', ['-jar', jarFilePath, parserPath, lexerPath, codePath], {
        cwd: basePath,
        stdio: ['inherit', 'pipe', 'pipe'] // stdin inherit, stdout and stderr as pipes
    });

    // Return the data that returns from the checker process
    // If it fails, throw an error
    // if it succeeds, try to parse the output as JSON
    let output = '';
    checkerProcess.stdout.on('data', (data) => {
        output += data;
    });

    let error = '';
    checkerProcess.stderr.on('data', (data) => {
        error += data;
    });

    const checker = await new Promise<ANTLRCheckerOutput>((resolve, reject) => {
        checkerProcess.on('close', (code) => {
            if (code === 0) {
                try {
                    const data: ANTLRCheckerOutput = JSON.parse(output);
                    resolve(data);
                } catch (e) {
                    reject(e);
                }
            } else {
                reject(new Error(`ANTLR grammar checker failed with code ${code}. Error: ${error}`));
            }
        });
    });

    // Create ANTLR errors
    const errors: ANTLRError[] = [];

    // Collect all warnings from that occured during the build of the grammar
    for (const warning of checker.warnings) {
        errors.push({
            grammarType: 'UNKNOWN',
            source: 'BUILD',
            message: warning['msg'],
            file: codePath,
        });
    }

    // Collect all errors from that occured during the build of the grammar
    for (const error of checker.lexer_grammar_errors) {
        errors.push({
            grammarType: 'LEXER',
            source: 'BUILD',
            message: error.msg,
            file: codePath,
            line: error.line,
            column: error.pos,
        });
    }

    for (const error of checker.parser_grammar_errors) {
        errors.push({
            grammarType: 'PARSER',
            source: 'BUILD',
            message: error.msg,
            file: codePath,
            line: error.line,
            column: error.pos,
        });
    }
    // Now, let's collect the errors that occured during the runtime of the grammar (under result.result)
    if (checker.result) {
        if (checker.result.parse_errors) {
            for (const error of checker.result.parse_errors) {
                errors.push({
                    grammarType: 'PARSER',
                    source: 'RUNTIME',
                    message: error.msg,
                    file: codePath,
                    line: error.line,
                    column: error.pos,
                });
            }
        }

        if (checker.result.lex_errors) {
            for (const error of checker.result.lex_errors) {
                errors.push({
                    grammarType: 'LEXER',
                    source: 'RUNTIME',
                    message: error.msg,
                    file: codePath,
                    line: error.line,
                    column: error.pos,
                });
            }
        }

        // If only 1 node was created, it means that the grammar was not parsed correctly
        if (checker.result.number_of_nodes < 2) {
            errors.push({
                grammarType: 'UNKNOWN',
                source: 'RUNTIME',
                message: `Grammar was not parsed correctly - only ${checker.result.number_of_nodes} node was created.`,
                file: codePath,
            });
        }
    }

    return errors;
}