import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { type ANTLRError } from '../syntactic/ErrorListener';
import esbuild from 'esbuild';

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
            ['-Dlanguage=JavaScript', ...grammarFilesWithExtension],
            {
                cwd: grammarDirectoryPath,
                stdio: ['inherit', 'pipe', 'pipe']
            }
        );

        let stdout = '';
        let stderr = '';

        process.stdout.on('data', (data) => { stdout += data.toString(); });
        process.stderr.on('data', (data) => { stderr += data.toString(); });

        process.on('close', () => { resolve({ stdout, stderr }); });
        process.on('error', (err) => {
            stderr = err.message;
            resolve({ stdout, stderr });
        });
    });
}

export async function compileANTLRFiles(grammarDirectoryPath: string): Promise<ANTLRError[]> {
    const grammarFiles = fs.readdirSync(grammarDirectoryPath);
    const grammarFilesWithExtension = grammarFiles.filter(file => file.endsWith('.g4'));

    // 1) Run ANTLR to produce .js files (ESM)
    let result = await spawnANTLR(grammarDirectoryPath, grammarFilesWithExtension);

    // 2) Check if there's a Python error, etc. (same as your code)
    if (result.stderr.includes('Traceback (most recent call last):')) {
        console.error('Got python error! Let\'s try recover...');
        result = await spawnANTLR(grammarDirectoryPath, grammarFilesWithExtension);
        if (result.stderr.includes('Traceback (most recent call last):')) {
            console.error('Recovery failed! Giving up...');
            process.exit(1);
        }
        console.log('Recovery succeeded!');
    }

    // 3) Now that .js (ESM) files exist, convert them to CJS
    await transpileESMtoCJS(grammarDirectoryPath);

    // 4) Parse any ANTLR errors
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

/**
 * Convert ANTLR-generated .js (ESM) into CommonJS via esbuild.
 */
async function transpileESMtoCJS(grammarDir: string) {
    // Find all .js files that ANTLR produced in grammarDir (MyParser.js, MyParserListener.js, etc.)
    const allFiles = fs.readdirSync(grammarDir);
    const jsFiles = allFiles.filter(f => f.endsWith('.js'));

    // Build absolute entryPoints array
    const entryPoints = jsFiles.map(f => path.join(grammarDir, f));

    // Use a single esbuild call for *all* files:
    try {
        await esbuild.build({
            entryPoints: [ path.join(grammarDir, "MyParser.js") ],
            outfile: path.join(grammarDir, "MyParser.cjs"),
            format: "cjs",
            bundle: true,           // Key: fully bundle references (including antlr4)
            platform: "node",
            target: "node16",
            sourcemap: false,
            minify: false,
            allowOverwrite: true,
            banner: {
                js: 'const import_meta_url = __filename;',
            },
            define: {
                'import.meta.url': 'import_meta_url'
            }
        });
        await esbuild.build({
            entryPoints: [ path.join(grammarDir, "MyLexer.js") ],
            outfile: path.join(grammarDir, "MyLexer.cjs"),
            format: "cjs",
            bundle: true,           // Key: fully bundle references (including antlr4)
            platform: "node",
            target: "node16",
            sourcemap: false,
            minify: false,
            allowOverwrite: true,
            banner: {
                js: 'const import_meta_url = __filename;',
            },
            define: {
                'import.meta.url': 'import_meta_url'
            }
        });

        /*const newCjsFiles = entryPoints.map(f => f.replace('.js', '.cjs'));
        newCjsFiles.forEach(file => {
            const fileContent = fs.readFileSync(file, 'utf8');
            const loadFix = fileContent.replace(/Listener.js/g, 'Listener.cjs');
            const extendFix = loadFix.replace(
                /import_antlr4\.default\.tree\.ParseTreeListener/g,
                'import_antlr4.default.ParseTreeListener'
            );
            fs.writeFileSync(file, extendFix);
        });*/
        console.log(`Converted ESM -> CJS for these ANTLR files:`, jsFiles);
    } catch (err) {
        console.error(`Failed to convert ESM -> CJS for grammar files in ${grammarDir}`, err);
    }
}
