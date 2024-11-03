import OpenAI from "openai";
import { type OpenAIEnv } from "./utils";
import { TimeoutError, timeout } from 'promise-timeout';
import { grammarGenerationSystemMessage } from "./prompts";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { Err, Ok, type Result } from "../result";

export type Grammar = {
    lexerSource: string;
    parserSource: string;
    generatedWithModel?: string;
};


export type Snippet = {
    snippet: string;
    fileName: string;
    filePath: string;
}

export const Stats = {
    totalRequests: 0,
    totalCompletedRequests: 0,
    inputTokens: 0,
    outputTokens: 0,
    score: new Map<string, number>(), // Map from model name to score
    get totalTokens () {
        return this.inputTokens + this.outputTokens;
    },
    get avgTokensPerRequest () {
        return this.totalTokens / this.totalRequests;
    },
    getCost(inputPricePerMillionTokens: number, outputPricePerMillionTokens: number) {
        return this.totalTokens * inputPricePerMillionTokens + this.totalTokens * outputPricePerMillionTokens;
    },
    addRequest() {
        this.totalRequests++;
    },
    addCompletedRequest(inputTokens: number, outputTokens: number) {
        this.totalCompletedRequests++;
        this.inputTokens += inputTokens;
        this.outputTokens += outputTokens;
    },
    addScore(modelName: string | undefined) {
        if (!modelName) {
            console.error('addScore: Model name is undefined');
            return;
        }
        if (this.score.has(modelName)) {
            this.score.set(modelName, this.score.get(modelName)! + 1);
        } else {
            this.score.set(modelName, 1);
        }
    },
}

function errorToString(error: ANTLRError, showGrammarType: boolean = true): string {
    const msg = error.message.replaceAll('\n', ' ');
    const firstPart = error.source === 'BUILD' ? 'while building' : 'under parsing';
    const grammarPart = showGrammarType ? ` in the ${error.grammarType.toLowerCase()} grammar` : '';
    return `Got error ${firstPart}${grammarPart}: ${msg}`;
}

function overlayErrorsOnCode(code: string, errors: ANTLRError[]): string {
    let newCodeLines = code.split('\n');
    const errorOnLineMap = new Map<number, ANTLRError[]>();
    // add errors to the errorOnLineMap
    for (const error of errors) {
        if (error.line === undefined) {
            throw new Error(`Error line number is undefined, this shouldn't happen!!!`);
        }
        const lineNumber = error.line - 1;
        // check if the line number is valid
        if (lineNumber < 0 || lineNumber >= newCodeLines.length) {
            throw new Error(`Invalid line number: ${lineNumber}, this shouldn't happen!!!`);
        }
        if (!errorOnLineMap.has(lineNumber)) {
            errorOnLineMap.set(lineNumber, []);
        }
        errorOnLineMap.get(lineNumber)!.push(error);
    }

    // for each line in map, add a comment with the errors
    for (const [lineNumber, errorsOnLine] of errorOnLineMap.entries()) {
        const line = newCodeLines[lineNumber];
        let allErrors = errorsOnLine.map(error => errorToString(error, false)).join(', ');
        const trimLength = 64;
        if (allErrors.length >= trimLength) {
            allErrors = allErrors.substring(0, trimLength-1) + '...';
        }
        const comment = `// Error: ${allErrors}`;
        newCodeLines[lineNumber] = `${line} ${comment}`;
    }
    return newCodeLines.join('\n');   
}

function constructPrompt(currentIntermediateSolution: Grammar, 
    codeSnippet: string, 
    errors: ANTLRError[],
    appendToMessage: string = ''
): OpenAI.Chat.Completions.ChatCompletionMessageParam[] {
    const messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = []

    // Add system message
    messages.push({
        role: 'system',
        content: grammarGenerationSystemMessage
    });

    // Add error messages
    // Overlay the error messages on the code by adding a comment
    const lexerErrorsWithLine = errors.filter(error => error.line !== undefined && error.grammarType === 'LEXER');
    const parserErrorsWithLine = errors.filter(error => error.line !== undefined && error.grammarType === 'PARSER');
    const lexerSource = overlayErrorsOnCode(currentIntermediateSolution.lexerSource, lexerErrorsWithLine);
    const parserSource = overlayErrorsOnCode(currentIntermediateSolution.parserSource, parserErrorsWithLine);

    // Put other error messages in its own block
    const otherErrorMessages = errors.filter(error => error.line === undefined || error.grammarType === 'UNKNOWN')
    let otherErrorsBlock = '';
    if (otherErrorMessages.length > 0) {
        otherErrorsBlock = `<OtherErrors>
${otherErrorMessages.map(error => `Under ${error.source}, in the ${error.grammarType} the following error occurred: ${error.message}`).join('\n')}
</OtherErrors>`;
    }

    // Add user message
    messages.push({
        role: 'user',
        content: `Here is my current ANTLR4 code:
<LexerGrammar>
\`\`\`antlr
${lexerSource}
\`\`\`
</LexerGrammar>
<ParserGrammar>
\`\`\`antlr
${parserSource}
\`\`\`
</ParserGrammar>
${otherErrorsBlock}
Complete the ANTLR4 lexer and parser grammars for the following code snippet. ${appendToMessage}
\`\`\`
${codeSnippet}
\`\`\`
Start by shortly thinking step-by-step.`
    });
    return messages;
}

function parseCompletionToGrammar(completion: string | null): Result<Grammar> {
    if (completion === null) {
        return Err('No completion provided');
    }
    // Find all ```antlr blocks
    const antlrBlocks = completion.match(/```antlr\n([\s\S]*?)```/g);
    if (antlrBlocks === null) {
        return Err('No Antlr blocks found in completion');
    }

    // Identify the lexer and parser blocks
    const lexerBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('lexer grammar'));
    const parserBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('parser grammar'));
    if (lexerBlock === undefined || parserBlock === undefined) {
        return Err('No lexer or parser block found in completion');
    }

    // Extract the lexer and parser to just the code without the Antlr block
    const lexerSource = lexerBlock.replace('```antlr\n', '').replace('```', '');
    const parserSource = parserBlock.replace('```antlr\n', '').replace('```', '');

    return Ok({
        lexerSource,
        parserSource,
    })
}

async function makeCompletionRequest(
    openaiEnv: OpenAIEnv,
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[],
    model: string,
    debug: undefined | 'input' | 'output' | 'both' = undefined,
    timeoutSeconds: number = 60,
): Promise<Result<Grammar>> {
    try {
        const openai = new OpenAI({
            baseURL: openaiEnv.baseUrl,
            apiKey: openaiEnv.apiKey,
        });
        const completionBody: OpenAI.Chat.Completions.ChatCompletionCreateParamsNonStreaming = {
            model: model,
            messages: messages,
            max_tokens: 4096,
            temperature: 0.8,
        };

        Stats.addRequest();
        const completionPromise = openai.chat.completions.create(completionBody); // { body: { ...completionBody, provider: { order: ['Hyperbolic'] } } }

        // Wrap the completion promise with a timeout
        const completion = await timeout(completionPromise, timeoutSeconds * 1000);
        const content = completion.choices[0].message.content;
        
        if (debug === 'input' || debug === 'both') console.log(messages);
        if (debug === 'output' || debug === 'both') console.log(content);
        
        const result = parseCompletionToGrammar(content);

        if (completion.usage) Stats.addCompletedRequest(completion.usage.prompt_tokens, completion.usage.completion_tokens);

        return result;
    } catch (error) {
        if (error instanceof TimeoutError) {
            return Err(`Request timed out after ${timeoutSeconds} seconds (model: ${model}).`);
        }
        console.error(error);
        return Err(`API request failed: ${error instanceof Error ? error.message : String(error)}`);
    }
}

export async function generateInitalGuess(openaiEnv: OpenAIEnv, snippets: Snippet[], initalLexer: string | undefined, initalParser: string | undefined, fileNamesThatDidntPass: string[] = []) {
    const combinedSnippets = snippets.map(snippet => {
        const didNotPass = fileNamesThatDidntPass.includes(snippet.filePath); // TODO: not the best way to do this
        const didNotPassString = didNotPass ? '// This snippet did not pass the grammar check!\n' : '';
        return `<File: ${snippet.fileName}>
${didNotPassString}
${snippet.snippet}
</File>`;
    }).join('\n');
    const tempSolution: Grammar = {
        lexerSource: initalLexer ?? 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE (make it as general as possible as the language is more complex than this snippet)\n',
        parserSource: initalParser ?? 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
    };
    const appendToMessage = "Write a complete solution by analyzing the semantics of the code and choosing the appropriate abstractions, and the use of generic rules. You are Terence Parr, the creator of ANTLR.";
    const messages = constructPrompt(tempSolution, combinedSnippets, [], appendToMessage);
    const completion = await makeCompletionRequest(openaiEnv, messages, openaiEnv.model, undefined, 60*5);
    return completion.unwrap();
}