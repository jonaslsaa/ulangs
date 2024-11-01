import fs from 'fs';
import { assert } from 'console';
import { callSWIProlog } from '../semantic/call';

import type { CLIGenerateArguments } from '../cli';
import { checkGrammar } from '../syntactic/check-grammar';
import { compileANTLRFiles } from '../syntactic/build';

export async function doCheck(file: string) {
    console.log(await checkGrammar('grammar/SimpleLangLexer.g4', 'grammar/SimpleLangParser.g4', file));
}

export async function doQuery(file: string, options: CLIGenerateArguments) {
    // TODO: check grammar first
    if (options.compileAntlr) {
        const errors = await compileANTLRFiles('grammar');
        if (errors.length === 0) {
            console.log('ANTLR files generated successfully');
        } else {
            console.error('ANTLR files generation failed:');
            errors.forEach(error => console.error(error));
            process.exit(1);
        }
    }

    // Import from parser.ts an prolog.ts
    const { createParserFromGrammar } = await import('../syntactic/parser');
    const { cstToAstGeneratorClauses, queryClauses } = await import('../semantic/prolog');

    const fileNoExt = file.replace(/\.[^/.]+$/, '');
    const outputPath = `${fileNoExt}.pl`;
    console.log("Generating prolog file from", file, "to", outputPath);

    const { parser, errorListener } = createParserFromGrammar(fs.readFileSync(file, 'utf8'));
    const tree = parser.program();

    // Check for errors after parsing
    if (errorListener.hasErrors()) {
        console.error("Parsing errors detected:");
        errorListener.getErrors().forEach(error => console.error(error));
        process.exit(1);
    }

    const includeConversionClauses = !options.excludeConversion;

    const clauses = [
        ...cstToAstGeneratorClauses(tree, parser, includeConversionClauses),
        ...queryClauses(options.query)
    ];
    assert(clauses.length > 0, "No clauses generated");
    assert(outputPath.length > 0, "No output path provided");
    assert(outputPath.endsWith('.pl'), "Output path must be a .pl file");
    fs.writeFileSync(outputPath, clauses.join('\n'));

    if (options.runProlog) {
        if (!includeConversionClauses) {
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
}