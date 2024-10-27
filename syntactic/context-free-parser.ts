import { CharStream, CommonTokenStream } from 'antlr4';
import { CustomErrorListener } from './ErrorListener';

export async function createParserFromGrammar(codeInput: string, generatedLexerG4Path: string, generatedParserG4Path: string) {
    const parserModulePath = generatedParserG4Path.replace(/\.g4$/, '.ts');
    const lexerModulePath = generatedLexerG4Path.replace(/\.g4$/, '.ts');
    const generatedParserModule = await import(parserModulePath);
    const generatedLexerModule = await import(lexerModulePath);

    // Get the classes from the generated modules
    const generatedParser = generatedParserModule.default;
    const generatedLexer = generatedLexerModule.default;
    
    const errorListener = new CustomErrorListener();

    const chars = new CharStream(codeInput);
    const lexer: any = new generatedLexer(chars);
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener);

    const tokens = new CommonTokenStream(lexer);
    const parser: any = new generatedParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(errorListener);

    // Enable error recovery mode
    parser.buildParseTrees = true;

    return {
        parser,
        errorListener,
    };
}