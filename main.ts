import fs from 'fs';
import { Command } from 'commander';
import { assert } from 'console';
import { callSWIProlog } from './semantic/call';
import { compileANTLRFiles } from './syntactic/build';

const cli = new Command();
cli.name('tbd');
cli.description('tbd');
cli.version('0.0.1');


type CLIGenerateArguments = {
    conversion: boolean | undefined;
    runProlog: boolean | undefined;
    query: string;
    generateAntlr: boolean | undefined
};

cli.command('generate')
    .description('Generates prolog facts from a given file')
    .argument('<file>', 'File to generate facts from')
    .argument('[output]', 'Output file')
    .option('-q, --query <query>', 'Query file to run after generating facts', "CstToAst/queries/printAST.pl")
    .option('-c, --conversion', 'Include conversion clauses', false)
    .option('-r, --run-prolog', 'Run prolog after generating facts', false)
    .option('-g, --generate-antlr', 'Generate ANTLR files', false)
    .action(async (file: string, output: string, options: CLIGenerateArguments) => {
        if (options.generateAntlr) {
            const errors = compileANTLRFiles('grammar');

            if (errors.length === 0) {
                console.log('ANTLR files generated successfully');
            } else {
                console.error('ANTLR files generation failed:');
                errors.forEach(error => console.error(error));
                process.exit(1);
            }
        }

        // Import from parser.ts an prolog.ts
        const { createParserFromGrammar } = await import('./syntactic/parser');
        const { cstToAstGeneratorClauses, queryClauses } = await import('./semantic/prolog');

        const fileNoExt = file.replace(/\.[^/.]+$/, '');
        const outputPath = output ?? `${fileNoExt}.pl`;
        console.log("Generating prolog file from", file, "to", outputPath);

        const { parser, errorListener } = createParserFromGrammar(fs.readFileSync(file, 'utf8'));
        const tree = parser.program();

        // Check for errors after parsing
        if (errorListener.hasErrors()) {
            console.error("Parsing errors detected:");
            errorListener.getErrors().forEach(error => console.error(error));
            process.exit(1);
        }

        const clauses = [
            ...cstToAstGeneratorClauses(tree, parser, options.conversion),
            ...queryClauses(options.query)
        ];
        assert(clauses.length > 0, "No clauses generated");
        assert(outputPath.length > 0, "No output path provided");
        assert(outputPath.endsWith('.pl'), "Output path must be a .pl file");
        fs.writeFileSync(outputPath, clauses.join('\n'));

        if (options.runProlog) {
            if (!options.conversion) {
                console.error("Cannot run prolog without conversion clauses");
                process.exit(1);
                return;
            }
            console.log("Running Prolog.");
            const prologResult = callSWIProlog(outputPath);
            if (prologResult.stderr) {
                console.error("Prolog failed:");
                console.error(prologResult.stderr);
            }
            const ast = prologResult.stdout.trim();
            if (ast === '') {
                console.error("Prolog returned empty result");
            } else {
                console.log(ast);
            }
        }
    });


cli.parse();