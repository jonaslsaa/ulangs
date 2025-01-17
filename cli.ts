import { Command } from 'commander';
import { doQuery } from './actions/query';
import { doInferGrammar, doVerboseCheck } from './actions/inferGrammar';
import fs from 'fs';
import assert from 'assert';
import { resolveRPC } from './actions/rpc';

const cli = new Command();
cli.name('ulangs-toolkit');
cli.description('Universal Language Server Toolkit');
cli.version('0.1.0');

export type CLIGenerateArguments = {
    adapter: string | undefined;
    query: string;
};

cli.command('query')
    .description('Generates Prolog from file with a given query')
    .argument('<target>', 'File to generate facts from')
    .argument('<lexer>', 'Lexer (path) file to use')
    .argument('<parser>', 'Parser (path) file to use')
    .option('-a, --adapter <adapter>', 'Adapter (path) file to use')
    .option('-q, --query <query>', 'Query file to run after generating facts', "printAST.pl")
    .action(async (target: string, lexer: string, parser: string, options: CLIGenerateArguments) => {
        assert(fs.existsSync(target), 'Target file does not exist');
        assert(fs.existsSync(lexer), 'Lexer file does not exist');
        assert(fs.existsSync(parser), 'Parser file does not exist');
        if (options.adapter) {
            assert(fs.existsSync(options.adapter), 'Adapter file does not exist');
        }
        doQuery(target, lexer, parser, options);
    });

cli.command('check')
    .description('Checks if source files can be parsed by given grammar')
    .argument('<directory>', 'Directory to check')
    .argument('<extension>', 'File extension to check, ex: .pyl')
    .argument('<lexer>', 'Lexer file to use')
    .argument('<parser>', 'Parser file to use')
    .action(async (directory: string, extension: string, lexer: string, parser: string) => {
        doVerboseCheck(directory, extension, lexer, parser);
    });


export type CLIInferGrammarArguments = {
    recursive: boolean;
    initialLexer: string | undefined;
    initialParser: string | undefined;
};

cli.command('infer-grammar')
    .description('Infer grammar from a directory of files')
    .argument('<directory>', 'Directory to infer grammar from')
    .argument('<extension>', 'File extension to infer grammar from, ex: .pyl')
    .argument('[outputDir]', 'Output directory for generated files', process.cwd())
    .option('-iL, --initial-lexer <path>', 'Use a file as a starting point for the lexer', undefined)
    .option('-iP, --initial-parser <path>', 'Use a file as a starting point for the parser', undefined)
    .option('-R, --recursive', 'Detect grammar from subdirectories', false)
    // .option('-s, --skip-first-guess', 'Skips the first guess and starts from the previous intermediate solution. Useful when inital grammar is almost correct.', false)
    .action(async (directory: string, extension: string, outputDir: string, options: CLIInferGrammarArguments) => {
        // Check if the directory exists
        if (!fs.existsSync(directory)) {
            console.error(`Directory ${directory} does not exist`);
            process.exit(1);
        }
        // Exit if extension does not start with a dot
        if (!extension.startsWith('.')) {
            console.error(`Extension ${extension} does not start with a dot`);
            process.exit(1);
        }

        // Check inital grammar files exist
        function checkFileExists(filePath: string | undefined) {
            if (filePath === undefined) return; // We don't care if it's undefined
            if (!fs.existsSync(filePath)) {
                console.error(`File '${filePath}' does not exist`);
                process.exit(1);
            }
        }
        checkFileExists(options.initialLexer);
        checkFileExists(options.initialParser);

        // Check if the output directory exists
        if (!fs.existsSync(outputDir)) {
            console.error(`Output directory '${outputDir}' does not exist`);
            process.exit(1);
        }

        doInferGrammar(directory, extension, outputDir, options);
    });


cli.command('rpc')
    .description('Remote procedure call')
    .argument('<function>', 'Function to call')
    .argument('[payload]', 'Payload to send (can be piped)')
    .action(async (functionName: string, payload?: string) => {
        // Check if we're receiving input from a pipe
        if (process.stdin.isTTY === undefined) {
            // Reading from pipe
            let data = '';
            process.stdin.setEncoding('utf8');
            
            process.stdin.on('data', chunk => {
                data += chunk;
            });
            
            process.stdin.on('end', () => {
                resolveRPC(functionName, data.trim());
            });
        } else {
            // Using command line argument
            if (!payload) {
                console.error('Error: Payload is required (when not piping input)');
                process.exit(1);
            }
            resolveRPC(functionName, payload);
        }
    });

cli.parse();