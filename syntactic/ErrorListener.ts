import { ErrorListener, Lexer, Parser, RecognitionException, Token } from 'antlr4';
import antlr4 from 'antlr4';
import type { AnyErrorWithLine } from '../llm/utils';


export type ANTLRError = {
    type: 'LEXER' | 'PARSER' | 'UNKNOWN';
    source: 'BUILD' | 'RUNTIME';
    message: string;
    file?: string;
    line?: number;
    column?: number;
};

export class CustomErrorListener extends ErrorListener<Token> {
    private errors: ANTLRError[] = [];

    syntaxError(
        recognizer: antlr4.Recognizer<Token | number>,
        offendingSymbol: Token | number,
        line: number,
        column: number,
        msg: string,
        e: RecognitionException
    ): void {
        console.error("ANTLR Runtime error:");
        console.error(`Line ${line}:${column} - ${msg}`);

        // Determine which file and what type of grammar is being parsed
        let file = '<unknown>';
        let type: ANTLRError['type'] = 'UNKNOWN';

        if (recognizer instanceof Lexer || recognizer instanceof Parser) {
            if (recognizer instanceof Lexer) type = 'LEXER';
            if (recognizer instanceof Parser) type = 'PARSER';
            const maybeFile = (recognizer as any)['grammarFileName'];
            if (maybeFile && typeof maybeFile === 'string' && maybeFile.length > 0) {
                file = maybeFile;
            }
        }

        this.errors.push({
            type: type,
            source: 'RUNTIME',
            message: msg,
            file: file,
            line,
            column
        });
    }

    getErrors(): ANTLRError[] {
        return this.errors;
    }

    hasErrors(): boolean {
        return this.errors.length > 0;
    }
}
