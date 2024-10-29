import { Command } from 'commander';
import { doCheck, doQuery } from './actions/query';
import { doInferGrammar } from './actions/inferGrammar';
import fs from 'fs';

const cli = new Command();
cli.name('tbd');
cli.description('tbd');
cli.version('0.0.1');

export type CLIGenerateArguments = {
    excludeConversion: boolean;
    runProlog: boolean;
    query: string;
    compileAntlr: boolean
};

cli.command('query')
    .description('Generates Prolog from file with a given query')
    .argument('<file>', 'File to generate facts from')
    .option('-q, --query <query>', 'Query file to run after generating facts', "CstToAst/queries/printAST.pl")
    .option('-ec, --exclude-conversion', 'Exclude conversion clauses', false)
    .option('-r, --run-prolog', 'Run prolog after generating facts', false)
    .option('-a, --compile-antlr', 'Generate ANTLR files', false)
    .action(async (file: string, options: CLIGenerateArguments) => {
        doQuery(file, options);
    });

cli.command('check')
    .description('Check if a file is syntactically valid')
    .argument('<file>', 'File to check')
    .action(async (file: string) => {
        doCheck(file);
    });


export type CLIInferGrammarArguments = {
    recursive: boolean;
    initialLexer: string | undefined;
    initialParser: string | undefined;
    skipFirstGuess: boolean;
};

cli.command('infer-grammar')
    .description('Infer grammar from a directory of files')
    .argument('<directory>', 'Directory to infer grammar from')
    .argument('<extension>', 'File extension to infer grammar from, ex: .pyl')
    .argument('[outputDir]', 'Output directory for generated files', process.cwd())
    .option('-iL, --initial-lexer <path>', 'Use a file as a starting point for the lexer', undefined)
    .option('-iP, --initial-parser <path>', 'Use a file as a starting point for the parser', undefined)
    .option('-R, --recursive', 'Detect grammar from subdirectories', false)
    .option('-s, --skip-first-guess', 'Skips the first guess and starts from the previous intermediate solution. Useful when inital grammar is almost correct.', false)
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


cli.parse();