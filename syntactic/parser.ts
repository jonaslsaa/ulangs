import { CharStream, CommonTokenStream } from 'antlr4';
import SimpleLangLexer from '../grammar/SimpleLangLexer';
import SimpleLangParser from '../grammar/SimpleLangParser';
import { CustomErrorListener } from './ErrorListener';

export function createParserFromGrammar(codeInput: string) {
    const errorListener = new CustomErrorListener();

    const chars = new CharStream(codeInput);
    const lexer = new SimpleLangLexer(chars);
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener);

    const tokens = new CommonTokenStream(lexer);
    const parser = new SimpleLangParser(tokens);
    parser.buildParseTrees = true;

    parser.removeErrorListeners();
    parser.addErrorListener(errorListener);
    
    return {
        parser,
        errorListener,
    };
}