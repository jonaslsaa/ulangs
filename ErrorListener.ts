import { ErrorListener, RecognitionException, Recognizer, Token } from 'antlr4';

export class CustomErrorListener extends ErrorListener<Token> {
    private errors: string[] = [];

    syntaxError(
        recognizer: Recognizer<Token | number>,
        offendingSymbol: Token | number,
        line: number,
        column: number,
        msg: string,
        e: RecognitionException
    ): void {
        this.errors.push(`Line ${line}:${column} - ${msg}`);
    }

    getErrors(): string[] {
        return this.errors;
    }

    hasErrors(): boolean {
        return this.errors.length > 0;
    }
}
