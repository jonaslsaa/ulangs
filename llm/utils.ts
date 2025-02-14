import { OpenAI } from 'openai';

export type OpenAIEnv = {
    apiKey: string;
    model: string;
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
    if (openaiApiKey === undefined || openaiModel === undefined) {
        throw new Error('OpenAI environment variables not set');
    }

    const r = {
        baseUrl: openaiBaseUrl,
        apiKey: openaiApiKey,
        model: openaiModel,
    };
    console.log("Loaded OpenAI environment variables. Using model:", r.model);
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