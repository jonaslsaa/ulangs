import { CharStream, CommonTokenStream, ParserRuleContext } from 'antlr4';
import { generatePrologFacts } from './prolog-generator/treeFacts';
import path from 'path';
import SimpleLangLexer from './grammar/SimpleLangLexer';
import SimpleLangParser from './grammar/SimpleLangParser';
import { CustomErrorListener } from './ErrorListener';
import fs from 'fs';

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

export function createParserFromGrammar(codeInput: string) {
    const errorListener = new CustomErrorListener();

    const chars = new CharStream(codeInput); // replace this with a FileStream as required
    const lexer = new SimpleLangLexer(chars);
    // Remove default console error listener
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener);
    const tokens = new CommonTokenStream(lexer);
    const parser = new SimpleLangParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(errorListener);
    return {
        parser,
        errorListener
    };
}

export function createTreeToAstGeneratorClauses(cstTree: ParserRuleContext,
                                                parser: SimpleLangParser,
                                                withConversionClaues: boolean = true,
                                                queryFile: string): string[] {
    
    function wrapWithComment(lines: string[], comment: string): string[] {
        return [
            `% ${comment}`,
            ...lines,
            ''
        ];
    }

    const basePath = path.join('rules', 'CstToAst');
    const basePathQueries = path.join(basePath, 'queries');
    const libraries = wrapWithComment(fileToLines(path.join(basePath, 'libraries.pl')), 'Libraries');
    const treeFacts = wrapWithComment(generatePrologFacts(cstTree, parser), 'Prolog facts for CST tree');
    const helpers = wrapWithComment(fileToLines(path.join(basePath, 'genericHelpers.pl')), 'Generic helpers');
    const conversion = wrapWithComment(fileToLines(path.join(basePath, 'conversion.pl')), 'CST to AST conversion rules');
    const query = wrapWithComment(fileToLines(path.join(basePathQueries, queryFile)), 'Main query');

    if (withConversionClaues) {
        return [...libraries, ...treeFacts, ...helpers, ...conversion, ...query];
    }
    return [
        ...libraries,
        ...treeFacts,
        ...helpers,
        '% CST Tree to AST conversion rules go here',
        '% Add conversion clauses here!',
        '% Following main query below must be callable with the conversion clauses:',
        '',
        ...query];
}