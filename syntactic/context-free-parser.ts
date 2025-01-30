import { CharStream, CommonTokenStream, Lexer, Parser, ParserRuleContext } from 'antlr4';
import { CustomErrorListener } from './ErrorListener';
import fs from 'fs';

export async function createParserFromGrammar(codeInput: string, generatedLexerG4Path: string, generatedParserG4Path: string) {
    const parserModulePath = generatedParserG4Path.replace(/\.g4$/, '.ts');
    const lexerModulePath = generatedLexerG4Path.replace(/\.g4$/, '.ts');
    // wait 0.5 seconds before importing the modules, as the files may not be ready yet
    await new Promise(resolve => setTimeout(resolve, 500));

    // Check if these files exist
    if (!fs.existsSync(parserModulePath) || !fs.existsSync(lexerModulePath)) {
        throw new Error(`Generated parser and lexer files not found at paths: ${parserModulePath} and ${lexerModulePath}`);
    }

    const generatedParserModule = await import(parserModulePath);
    const generatedLexerModule = await import(lexerModulePath);

    // Get the classes from the generated modules
    const generatedParser = generatedParserModule.default;
    const generatedLexer = generatedLexerModule.default;
    
    const errorListener = new CustomErrorListener();

    const chars = new CharStream(codeInput);
    const lexer: Lexer = new generatedLexer(chars);
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener);

    type ParserWithProgramStartRule = Parser & { program: () => ParserRuleContext };

    const tokens = new CommonTokenStream(lexer);
    const parser: ParserWithProgramStartRule = new generatedParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(errorListener);

    parser.buildParseTrees = true;

    return {
        parser,
        errorListener,
    };
}