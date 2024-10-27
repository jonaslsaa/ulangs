import { ErrorListener, RecognitionException, Recognizer, Token } from 'antlr4';

export type ANTLRError = {
    isWarning: boolean;
    grammarType: 'LEXER' | 'PARSER' | 'UNKNOWN';
    source: 'BUILD' | 'RUNTIME';
    message: string;
    file?: string;
    line?: number;
    column?: number;
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

        // TODO: determine which file is being parsed
        const file = '<unknown>';

        this.errors.push({
            isWarning: false,
            grammarType: type,
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
