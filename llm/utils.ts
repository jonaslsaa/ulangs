import { OpenAI } from 'openai';

export type OpenAIEnv = {
    apiKey: string;
    model: string;
    soModel: string; /* Structured output model */
    baseUrl?: string;
};

export function loadOpenAIEnvVars(): OpenAIEnv {
    function getEnvVar(name: string): string | undefined {
        const value = process.env[name];
        if (value === undefined || value === '') {
            return undefined;
        }
        return value;
    }

    const openaiBaseUrl = getEnvVar('OPENAI_COMPATIBLE_BASEURL');
    const openaiApiKey = getEnvVar('OPENAI_COMPATIBLE_API_KEY');
    const openaiModel = getEnvVar('OPENAI_COMPATIBLE_MODEL');
    let openaiSOModel = getEnvVar('OPENAI_COMPATIBLE_SO_MODEL');

    if (openaiApiKey === undefined || openaiModel === undefined) {
        throw new Error('OpenAI environment variables not set');
    }

    if (openaiSOModel === undefined) {
        openaiSOModel = openaiModel;
    }

    const r = {
        baseUrl: openaiBaseUrl,
        apiKey: openaiApiKey,
        model: openaiModel,
        soModel: openaiSOModel,
    };
    console.log("Loaded OpenAI environment variables. Using model:", r.model, "(Structured output model: " + r.soModel + ")");
    // console.log(r.baseUrl, r.apiKey.substring(0, 8) + '...', r.model);
    return r;
}


export function stripSystemMessage(messages: OpenAI.Chat.ChatCompletionMessageParam[]): OpenAI.Chat.ChatCompletionMessageParam[] {
    return messages.filter(m => m.role !== 'system');
}

export type OpenAIMessage = OpenAI.Chat.Completions.ChatCompletionMessageParam;

export function midpoint<T>(arr: T[]): T | undefined {
	if (arr.length === 0) return undefined;
	if (arr.length === 1) return arr[0];
	if (arr.length === 2) return arr[0];
	return arr[Math.floor(arr.length / 2)];
}

export type AnyErrorWithLine = { line?: number, message: string, type: string };

export function overlayErrorsOnCode<T extends AnyErrorWithLine>(code: string, errors: T[]): string {
    let newCodeLines = code.split('\n');
    const errorOnLineMap = new Map<number, T[]>();
    // add errors to the errorOnLineMap
    for (const error of errors) {
        if (error.line === undefined) {
            throw new Error(`Error line number is undefined, this really shouldn't happen!!!: ${error.message}`);
        }
        const lineNumber = error.line - 1;
        // check if the line number is valid
        if (lineNumber < 0 || lineNumber >= newCodeLines.length) {
            console.warn(`Invalid line number: ${lineNumber}: ${error.message}, file length: ${newCodeLines.length}`);
            //error.type = 'UNKNOWN';
            //throw new Error(`Invalid line number: ${lineNumber}: ${error.message}`);
        }
        if (!errorOnLineMap.has(lineNumber)) {
            errorOnLineMap.set(lineNumber, []);
        }
        errorOnLineMap.get(lineNumber)!.push(error);
    }

    // for each line in map, add a comment with the errors
    for (const [lineNumber, errorsOnLine] of errorOnLineMap.entries()) {
        const line = newCodeLines[lineNumber];
        // todo: de-dupe errors
        let allErrors = errorsOnLine.map(error => error.message).join(', ');
        const trimLength = 256;
        if (allErrors.length >= trimLength) {
            allErrors = allErrors.substring(0, trimLength-1) + '...';
        }
        const comment = `// ANTLR Error: ${allErrors}`;
        newCodeLines[lineNumber] = `${line} ${comment}`;
    }
    return newCodeLines.join('\n');   
}