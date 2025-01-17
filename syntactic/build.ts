import { spawn } from 'child_process';
import fs from 'fs';
import { type ANTLRError } from '../syntactic/ErrorListener';

function toANTLRError(_error: string): ANTLRError {
    const error = _error.trim();

    const isError = error.startsWith('error(');
    let isWarning = error.startsWith('warning(');

    if (!isError && !isWarning) {
        console.error(`Unknown error format: ${error}`);
    }

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

    let errorType: ANTLRError['grammarType'];
    if (error.toLowerCase().includes('lexer')) {
        errorType = 'LEXER';
    } else if (error.toLowerCase().includes('parser')) {
        errorType = 'PARSER';
    } else {
        console.error(`Unknown grammar type: ${error}`);
        errorType = 'UNKNOWN';
    }

    const messageRegex = /: (.+):/;
    const messageMatch = messageRegex.exec(error);
    let message = error;
    if (messageMatch) {
        message = messageMatch[1];
    }

    return {
        grammarType: errorType,
        source: 'BUILD',
        message: error,
        file: file,
        line: line,
        column: column,
    };
}

async function spawnANTLR(grammarDirectoryPath: string, grammarFilesWithExtension: string[]): Promise<{ stdout: string; stderr: string }> {
    return new Promise((resolve) => {
        const process = spawn('antlr4',
            ['-Dlanguage=TypeScript', ...grammarFilesWithExtension],
            {
                cwd: grammarDirectoryPath,
                stdio: ['inherit', 'pipe', 'pipe'] // stdin inherit, stdout and stderr as pipes
            }
        );

        let stdout = '';
        let stderr = '';

        process.stdout.on('data', (data) => {
            stdout += data.toString();
        });

        process.stderr.on('data', (data) => {
            stderr += data.toString();
        });

        // Always resolve with the output, regardless of exit code
        process.on('close', () => {
            resolve({ stdout, stderr });
        });

        process.on('error', (err) => {
            stderr = err.message;
            resolve({ stdout, stderr });
        });
    });
}

export async function compileANTLRFiles(grammarDirectoryPath: string): Promise<ANTLRError[]> {
    const grammarFiles = fs.readdirSync(grammarDirectoryPath);
    const grammarFilesWithExtension = grammarFiles.filter(file => file.endsWith('.g4'));

    // First attempt
    let result = await spawnANTLR(grammarDirectoryPath, grammarFilesWithExtension);
    
    // Check if it's a Python error and try recovery
    if (result.stderr.includes('Traceback (most recent call last):')) {
        console.error('Got python error! Let\'s try recover...');
        // Recovery attempt
        result = await spawnANTLR(grammarDirectoryPath, grammarFilesWithExtension);
        if (result.stderr.includes('Traceback (most recent call last):')) {
            console.error('Recovery failed! Giving up...');
            process.exit(1);
        }
        console.log('Recovery succeeded!');
    }

    return parseErrors(result.stderr, grammarDirectoryPath);
}

function parseErrors(stderr: string, grammarDirectoryPath: string): ANTLRError[] {
    if (stderr) {
        console.log("ANTLR4 stderr in directory", grammarDirectoryPath, ":");
        console.log(stderr);
        
        return stderr
            .split('\n')
            .map(line => line.trim())
            .filter(line => line.length > 0)
            .map(toANTLRError);
    }
    return [];
}