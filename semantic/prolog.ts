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
        `% === ${comment} ===`,
        ...lines,
        ''
    ];
}

export function getApplicableTreeClauses(cstTree: ParserRuleContext,
                                parser: Parser,
                                adapterPathOrContent: string | undefined,
                                isAdapterContent: boolean = false,
                                ): string[] {

    const builtinBasePath = path.join('rules', 'adapter');
    
    const libraries = wrapWithComment(fileToLines(path.join(builtinBasePath, 'libraries.pl')), 'Libraries.pl')
    const treeFacts = wrapWithComment(generatePrologFacts(cstTree, parser), 'Auto-generated tree clauses')
    const helpers = wrapWithComment(fileToLines(path.join(builtinBasePath, 'genericHelpers.pl')), 'Helpers.pl')

    if (adapterPathOrContent) {
        let adapterClauses: string[] = [];
        if (isAdapterContent) {
            adapterClauses = adapterPathOrContent.split('\n');
        } else {
            adapterClauses = fileToLines(adapterPathOrContent);
        }
        const adapter = wrapWithComment(adapterClauses, 'Adapter'); // TODO: change comment
        return [...libraries, ...treeFacts, ...helpers, ...adapter];
    }
    return [
        ...libraries,
        ...treeFacts,
        ...helpers,
        ...wrapWithComment(fileToLines(path.join(builtinBasePath, 'adapter_template.pl')), 'Adapter template (replace me!)')
    ];
}

export function getQueryClauses(queryFile: string): string[] {
    const basePathQueries = path.join('rules', 'queries');
    if (!fs.existsSync(basePathQueries)) {
        console.error(`Query file ${queryFile} not found`);
        process.exit(1);
    }
    const query = wrapWithComment(fileToLines(path.join(basePathQueries, queryFile)), 'Main query');
    return query;
}