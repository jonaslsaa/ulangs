import type { Parser, ParserRuleContext } from "antlr4";
import path from 'path';
import { generatePrologFacts } from "../prolog-generator/cst";
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

function wrapWithComment(lines: string[], comment: string): string[] {
    return [
        `% ${comment}`,
        ...lines,
        ''
    ];
}

export function cstToAstGeneratorClauses(cstTree: ParserRuleContext,
                                            parser: Parser,
                                            withConversionClaues: boolean = true
                                        ): string[] {

    const basePath = path.join('rules', 'CstToAst');
    
    const libraries = wrapWithComment(fileToLines(path.join(basePath, 'libraries.pl')), 'Libraries');
    const treeFacts = wrapWithComment(generatePrologFacts(cstTree, parser), 'Prolog facts for CST tree');
    const helpers = wrapWithComment(fileToLines(path.join(basePath, 'genericHelpers.pl')), 'Generic helpers');
    const conversion = wrapWithComment(fileToLines(path.join(basePath, 'conversion.pl')), 'CST to AST conversion rules');

    if (withConversionClaues) {
        return [...libraries, ...treeFacts, ...helpers, ...conversion];
    }
    return [
        ...libraries,
        ...treeFacts,
        ...helpers,
        '% CST Tree to AST conversion rules go here',
        '% Add conversion clauses here!',
        '% Following main query below must be callable with the conversion clauses:',
        '',
    ];
}

export function queryClauses(queryFile: string): string[] {
    console.log("Query file:", queryFile);
    const basePathQueries = path.join('rules');
    const query = wrapWithComment(fileToLines(path.join(basePathQueries, queryFile)), 'Main query');
    return query;
}