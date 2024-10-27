import { spawnSync } from 'child_process';
import fs from 'fs';

export function compileANTLRFiles(grammarDirectoryPath: string): string[] {
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
        errors.push(...result.stderr
            .split('\n')
            .map(line => line.trim())
            .filter(line => line.length > 0)); // Remove empty lines
    }

    return errors;
}