import fs from 'fs';
import { assert } from 'console';
import { callSWIProlog } from '../semantic/call';

import type { CLIGenerateArguments } from '../cli';
import { compileANTLRFiles } from '../syntactic/build';
import { _createWorkingDirectory, _createTemporaryFile } from './utils/io';
import { checkGrammar } from '../syntactic/check-grammar.ts';
import path from 'path';

export async function doQuery(targetPath: string, lexerPath: string, parserPath: string, options: CLIGenerateArguments) {

    // Create temporary directory to put our generated files in
    const workingDirectory = _createWorkingDirectory('query');

    // Check grammar is actually syntactically valid for our target (using our java parser)
    const antlrJavaErrors = await checkGrammar(lexerPath, parserPath, targetPath);
    if (antlrJavaErrors.length > 0) {
        console.error('Grammar is not syntactically valid:');
        antlrJavaErrors.forEach(error => console.error(error));
        process.exit(1);
    }
    
    // Copy .g4 files to tmpANTLR
    const grammarPaths = {
        lexer: path.join(workingDirectory, 'MyLexer.g4'),
        parser: path.join(workingDirectory, 'MyParser.g4'),
    }
    fs.copyFileSync(lexerPath, grammarPaths.lexer);
    fs.copyFileSync(parserPath, grammarPaths.parser);
    // Compile antlr files to typescript
    const antlrCompilationErrors = await compileANTLRFiles(workingDirectory);
    if (antlrCompilationErrors.length === 0) {
        console.log('ANTLR generated successfully');
    } else {
        console.error('ANTLR generation failed:');
        antlrCompilationErrors.forEach(error => console.error(error));
        process.exit(1);
    }

    // Dynamically import parser and prolog functions
    const { createParserFromGrammar } = await import('../syntactic/context-free-parser.ts');
    const { cstToAstGeneratorClauses, queryClauses } = await import('../semantic/prolog');

    // Create a parser from grammar
    const targetContent = fs.readFileSync(targetPath, 'utf8');
    const { parser, errorListener } = await createParserFromGrammar(targetContent, grammarPaths.lexer, grammarPaths.parser);
    const tree = parser.program();

    // Check for errors after parsing
    console.log("Target:", targetPath);
    if (errorListener.hasErrors()) {
        console.error("Parsing errors detected:");
        errorListener.getErrors().forEach(error => console.error(error));
        process.exit(1);
    }
    console.log("Target parsed successfully.");

    // Compose clauses for prolog file
    const clauses = [
        ...cstToAstGeneratorClauses(tree, parser, options.adapter),
        ...queryClauses(options.query)
    ];
    assert(clauses.length > 0, "No clauses generated");

    // Generate prolog file
    const outputPath = path.join(workingDirectory, 'query.pl');
    assert(outputPath.length > 0, "No output path provided");
    assert(outputPath.endsWith('.pl'), "Output path must be a .pl file");
    fs.writeFileSync(outputPath, clauses.join('\n'));

    // Run prolog (if adapter is included)
    if (options.adapter === undefined) {
        console.warn("WARN: Since no adapter was provided, the query will not be run.");
        console.log("Generated prolog file:", outputPath);
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