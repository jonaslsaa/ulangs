import { Command } from 'commander';
import { doQuery } from './main/query';


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

cli.parse();