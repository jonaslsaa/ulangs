import { CharStream, CommonTokenStream, ParserRuleContext } from 'antlr4';
import SimpleLangLexer from './grammar/SimpleLangLexer';
import SimpleLangParser from './grammar/SimpleLangParser';
import { generatePrologFacts } from './prolog-generator/treeFacts';
import path from 'path';
import fs from 'fs';
import { Command } from 'commander';
import { assert } from 'console';
import { callSWIProlog } from './utils';


function fileToLines(fileRelativePath: string): string[] {
    const filePath = path.join(process.cwd(), fileRelativePath);
    try {
        const fileContent = fs.readFileSync(filePath, 'utf8');
        return fileContent.split('\n');
    } catch (error) {
        console.error(`Failed to read file ${filePath}: ${error}`);
        process.exit(1);
    }
}

function createParserFromGrammar(codeInput: string) {
    const chars = new CharStream(codeInput); // replace this with a FileStream as required
    const lexer = new SimpleLangLexer(chars);
    const tokens = new CommonTokenStream(lexer);
    return new SimpleLangParser(tokens);
}

function createTreeToAstGeneratorClauses(cstTree: ParserRuleContext, parser: SimpleLangParser, withConversionClaues: boolean = true, queryFile: string): string[] {
    function wrapWithComment(lines: string[], comment: string): string[] {
        return [
            `% ${comment}`,
            ...lines,
            ''
        ];
    }

    const baseRelPath = path.join('rules', 'CstToAst');
    const libraries = wrapWithComment(fileToLines(path.join(baseRelPath, 'libraries.pl')), 'Libraries');
    const treeFacts = wrapWithComment(generatePrologFacts(cstTree, parser), 'Prolog facts for CST tree');
    const helpers = wrapWithComment(fileToLines(path.join(baseRelPath, 'genericHelpers.pl')), 'Generic helpers');
    const conversion = wrapWithComment(fileToLines(path.join(baseRelPath, 'conversion.pl')), 'CST to AST conversion rules');
    const query = wrapWithComment(fileToLines(path.join(baseRelPath, queryFile)), 'Main query');

    if (withConversionClaues) {
        return [...libraries, ...treeFacts, ...helpers, ...conversion, ...query];
    }
    return [
        ...libraries,
        ...treeFacts,
        ...helpers,
        '% CST Tree to AST conversion rules go here',
        '% Add conversion clauses here!',
        '% Following main query below must be callable with the conversion clauses',
        ...query];
}


const cli = new Command();
cli.name('tbd');
cli.description('tbd');
cli.version('0.0.1');

cli.command('generate')
    .description('Generates prolog facts from a given file')
    .argument('<file>', 'File to generate facts from')
    .argument('[output]', 'Output file')
    .option('-q, --query <query>', 'Query to run after generating facts', "mainQuery.pl")
    .option('-c, --conversion', 'Include conversion clauses', false)
    .option('-r, --run-prolog', 'Run prolog after generating facts', false)
    .action(async (file: string, output: string, options: { conversion: boolean | undefined, runProlog: boolean | undefined, query: string }) => {
        const fileNoExt = file.replace(/\.[^/.]+$/, '');
        const outputPath = output ?? `${fileNoExt}.pl`;
        console.log("Generating prolog file from", file, "to", outputPath);

        const parser = createParserFromGrammar(fs.readFileSync(file, 'utf8'));
        const tree = parser.program();

        const clauses = createTreeToAstGeneratorClauses(tree, parser, options.conversion, options.query);
        assert(clauses.length > 0, "No clauses generated");
        assert(outputPath.length > 0, "No output path provided");
        assert(outputPath.endsWith('.pl'), "Output path must be a .pl file");
        fs.writeFileSync(outputPath, clauses.join('\n'));
        console.log(`Generated facts written to ${outputPath}`);

        if (options.runProlog) {
            if (!options.conversion) {
                console.error("Cannot run prolog without conversion clauses");
                process.exit(1);
                return;
            }
            console.log("Running prolog to generate AST:");
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