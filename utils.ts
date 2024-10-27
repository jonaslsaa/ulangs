
import { spawnSync } from 'child_process';
import type { SpawnSyncReturns, SpawnSyncOptionsWithStringEncoding } from 'child_process';
import fs from 'fs';

export function callSWIProlog(file: string): {stdout: string, stderr: string} {
    try {
        const result: SpawnSyncReturns<string> = spawnSync('swipl', ['-q', '-s', file, '-t', 'halt'], {
            encoding: 'utf8',
            stdio: 'pipe'
        } as SpawnSyncOptionsWithStringEncoding);

        if (result.error) {
            // result.error is NodeJS.ErrnoException
            throw new Error(`Failed to execute swipl: ${result.error.message}`);
        }

        if (result.status !== 0) {
            throw new Error(`Prolog process exited with code ${result.status}: ${result.stderr}`);
        }

        return {
            stdout: result.stdout || '',
            stderr: result.stderr || ''
        };
    } catch (error) {
        // Type guard for better error handling
        if (error instanceof Error) {
            throw error;
        }
        throw new Error(`Failed to run SWI-Prolog: ${String(error)}`);
    }
}

export function generateANTLRFiles(grammarDirectoryPath: string): void {
    // We have to detect all .g4 files in the directory and generate their corresponding files with:
    // antlr4 -Dlanguage=TypeScript SimpleLangLexer.g4 SimpleLangParser.g4
    const grammarFiles = fs.readdirSync(grammarDirectoryPath);
    const grammarFilesWithExtension = grammarFiles.filter(file => file.endsWith('.g4'));
    
    // Call ANTLR for all the files at once with CD in the grammar directory
    spawnSync('antlr4', ['-Dlanguage=TypeScript', ...grammarFilesWithExtension], {
        cwd: grammarDirectoryPath,
        encoding: 'utf8',
        stdio: 'inherit'
    });
    console.log('ANTLR files generated successfully');
}