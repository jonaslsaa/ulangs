import { ErrorListener, RecognitionException, Recognizer, Token } from 'antlr4';

export type ANTLRError = {
    type: 'LEXER' | 'PARSER';
    source: 'BUILD' | 'RUNTIME';
    message: string;
    line: number;
    column: number;
};

export class CustomErrorListener extends ErrorListener<Token> {
    private errors: ANTLRError[] = [];

    syntaxError(
        recognizer: Recognizer<Token | number>,
        offendingSymbol: Token | number,
        line: number,
        column: number,
        msg: string,
        e: RecognitionException
    ): void {
        console.log("ANTLR Runtime error:");
        console.log(recognizer, offendingSymbol, line, column, msg, e);
        console.error(`Line ${line}:${column} - ${msg}`);

        // TODO: determine if this is a lexer or parser error
        const type = 'PARSER';

        this.errors.push({
            type: type,
            source: 'RUNTIME',
            message: msg,
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
