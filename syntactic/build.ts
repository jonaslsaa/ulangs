import { spawnSync } from 'child_process';
import fs from 'fs';
import { type ANTLRError } from '../syntactic/ErrorListener';

function toANTLRError(_error: string): ANTLRError {
    /* Error format:
        error(50): SimpleLangLexer.g4:11:0: ...message...
        warning(125): SimpleLangParser.g4:25:15: ...message...
        error(12): OtherFileLexer.g4:11:0: ...message...
    */
    // Match error type, file, line, column, and message
    const error = _error.trim();

    const isError = error.startsWith('error(');
    let isWarning = error.startsWith('warning(');

    if (!isError && !isWarning) {
        console.error(`Unknown error format: ${error}`);
        isWarning = true; // Default to warning
    }

    // Extract file, line, and column using regex
    const fileRegex = /: (.+):(\d+):(\d+):/;
    const match = fileRegex.exec(error);
    let file: string | undefined;
    let line: number | undefined;
    let column: number | undefined;

    if (match) {
        file = match[1];
        try {
            line = parseInt(match[2]);
            column = parseInt(match[3]);
        } catch (error) {
            console.error(`Failed to parse line and column: ${error}`);
            line = undefined;
            column = undefined;
        }
    }

    // Detect grammar type based on error message
    let errorType: ANTLRError['grammarType'];
    if (error.toLowerCase().includes('lexer')) {
        errorType = 'LEXER';
    } else if (error.toLowerCase().includes('parser')) {
        errorType = 'PARSER';
    } else {
        console.error(`Unknown grammar type: ${error}`);
        errorType = 'UNKNOWN';
        isWarning = true;
    }

    // Try to extract the error message
    const messageRegex = /: (.+):/;
    const messageMatch = messageRegex.exec(error);
    let message = error;
    if (messageMatch) {
        message = messageMatch[1];
    }
    // TODO: consider using message

    return {
        isWarning: isWarning,
        grammarType: errorType,
        source: 'BUILD',
        message: error,
        file: file,
        line: line,
        column: column,
    };
}

export function compileANTLRFiles(grammarDirectoryPath: string): ANTLRError[] {
    const grammarFiles = fs.readdirSync(grammarDirectoryPath);
    const grammarFilesWithExtension = grammarFiles.filter(file => file.endsWith('.g4'));

    // Use spawnSync with pipe to capture output
    const result = spawnSync('antlr4',
        ['-Dlanguage=TypeScript', ...grammarFilesWithExtension],
        {
            cwd: grammarDirectoryPath,
            encoding: 'utf8',
            stdio: ['inherit', 'pipe', 'pipe'] // stdin inherit, stdout and stderr as pipes
        }
    );

    // Parse stderr into array of errors
    const errors: string[] = [];
    if (result.stderr) {
        console.log("ANTLR4 stderr in directory", grammarDirectoryPath, ":");
        console.log(result.stderr);
        errors.push(...result.stderr
            .split('\n')
            .map(line => line.trim())
            .filter(line => line.length > 0)); // Remove empty lines
    }

    return errors.map(toANTLRError);
}