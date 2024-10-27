import { CharStream, CommonTokenStream, ParserRuleContext } from 'antlr4';
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
    parser.removeErrorListeners();
    parser.addErrorListener(errorListener);

    // Enable error recovery mode
    parser.buildParseTrees = true;

    return {
        parser,
        errorListener,
    };
}