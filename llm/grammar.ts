import OpenAI from "openai";
import { type OpenAIEnv, type OpenAIMessage, loadOpenAIEnvVars } from './utils';
import { TimeoutError, timeout } from 'promise-timeout';
import { grammarGenerationDeveloperMessage } from "./prompts";
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

export type TestedSnippet = {
    onSnippet: Snippet;
    usedGrammar: Grammar;
    errors?: ANTLRError[];
    success: boolean;
};

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
    }
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
        // todo: de-dupe errors
        let allErrors = errorsOnLine.map(error => error.message).join(', ');
        const trimLength = 256;
        if (allErrors.length >= trimLength) {
            allErrors = allErrors.substring(0, trimLength-1) + '...';
        }
        const comment = `// Error: ${allErrors}`;
        newCodeLines[lineNumber] = `${line} ${comment}`;
    }
    return newCodeLines.join('\n');   
}

export function constructPrompt(currentIntermediateSolution: Grammar,
                        firstNonWorkingTestedSnippet: TestedSnippet,
                        allTestedSnippets: Snippet[] | undefined,
                        messages: OpenAIMessage[],
                        repairMode: boolean = false,
                        includeErrors: boolean = false,
                        appendToMessage: string = ''
): OpenAI.Chat.Completions.ChatCompletionMessageParam[] {
    if (allTestedSnippets?.length === 0) {
        throw new Error('No tested snippets given.');
    }

    // Add system message
    if (messages.length === 0) {
        messages.push({
            role: 'user',
            content: grammarGenerationDeveloperMessage
        },
        {
            role: 'assistant',
            content: 'Understood, I\'m ready.'
        });
    }

    let lexerSource = currentIntermediateSolution.lexerSource;
    let parserSource = currentIntermediateSolution.parserSource;
    let snippetWithErrors = firstNonWorkingTestedSnippet.onSnippet.snippet;
    let strThereAreErrorsInSnippets = '';
    let strThereAreErrorsInLexerOrParser = '';
    let otherErrorsBlock = '';

    if (includeErrors) {
        const errors = firstNonWorkingTestedSnippet.errors;
        const codeSnippet = firstNonWorkingTestedSnippet.onSnippet.snippet;
        if (errors == undefined) throw new Error('No errors found in firstNonWorkingTestedSnippet');

        // Add error messages
        // Overlay the error messages on the code by adding a comment
        const lexerErrorsWithLine = errors.filter(error => error.line !== undefined && error.grammarType === 'LEXER' && error.source === 'BUILD'); // TODO: i think RUNTIME is wrong to overlay on lexer/parser
        const parserErrorsWithLine = errors.filter(error => error.line !== undefined && error.grammarType === 'PARSER' && error.source === 'BUILD');
        lexerSource = overlayErrorsOnCode(currentIntermediateSolution.lexerSource, lexerErrorsWithLine);
        parserSource = overlayErrorsOnCode(currentIntermediateSolution.parserSource, parserErrorsWithLine);

        const runtimeErrorsWithLine = errors.filter(error => error.line !== undefined && error.grammarType === 'PARSER' && error.source === 'RUNTIME');

        const remainingErrors = errors.filter(error => !(lexerErrorsWithLine.includes(error) || parserErrorsWithLine.includes(error) || runtimeErrorsWithLine.includes(error)));

        // Overlay the runtime errors on the code snippet
        snippetWithErrors = overlayErrorsOnCode(codeSnippet, runtimeErrorsWithLine);

        // Put other error messages in its own block
        if (remainingErrors.length > 0) {
            otherErrorsBlock = `<OtherErrors>
    ${remainingErrors.map(error => `Under ${error.source}, in the ${error.grammarType} the following error occurred: ${error.message}`).join('\n')}
    </OtherErrors>`;
        }

        const isLexerOrParserErrors = lexerErrorsWithLine.length > 0 || parserErrorsWithLine.length > 0;
        const isRuntimeErrors = runtimeErrorsWithLine.length > 0;

        strThereAreErrorsInSnippets = isRuntimeErrors ? '(I\'ve put the errors in // comments)' : '';
        strThereAreErrorsInLexerOrParser = isLexerOrParserErrors ? '(I\'ve put the errors in // comments)' : '';
    }

    const endMsg = repairMode ? 'What\'s wrong here? Repair my ANTLR4 lexer and parser grammars so that the code snippets can be parsed.'
                            : 'Write the complete ANTLR4 lexer and parser grammars so that all the code snippets can be parsed!';

    // Add user message
    messages.push({
        role: 'user',
        content: `<AllCodeSnippets>
${allTestedSnippets ? allTestedSnippets.map(s => s.snippet).join('\n\n=== next file ===\n\n') : '... previously given ...'}
</AllCodeSnippets>

Right now, I'm trying to parse the following code snippet ${strThereAreErrorsInSnippets}:
\`\`\`
${snippetWithErrors}
\`\`\`
Here is my current ANTLR4 code ${strThereAreErrorsInLexerOrParser}:
<MyLexer.g4>
\`\`\`antlr
${lexerSource}
\`\`\`
</MyLexer.g4>
<MyParser.g4>
\`\`\`antlr
${parserSource}
\`\`\`
</MyParser.g4>
${otherErrorsBlock}

${endMsg} ${appendToMessage}
`
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

export async function makeCompletionRequest(
    openaiEnv: OpenAIEnv,
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[],
    model: string,
    debug: undefined | 'input' | 'output' | 'both' = undefined,
    timeoutSeconds: number = 240,
): Promise<Result<Grammar>> {
    try {
        const openai = new OpenAI({
            baseURL: openaiEnv.baseUrl,
            apiKey: openaiEnv.apiKey,
        });
        const completionBody: OpenAI.Chat.Completions.ChatCompletionCreateParamsNonStreaming = {
            model: model,
            messages: messages,
            max_completion_tokens: 32000,
            //temperature: 0.8,
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

        // Add the assistant completion to the messages
        messages.push({
            role: 'assistant',
            content: content
        });

        return result;
    } catch (error) {
        if (error instanceof TimeoutError) {
            return Err(`Request timed out after ${timeoutSeconds} seconds (model: ${model}).`);
        }
        console.error(error);
        return Err(`API request failed: ${error instanceof Error ? error.message : String(error)}`);
    }
}

export async function generateInitalGuess(openaiEnv: OpenAIEnv,
                                        firstNonWorkingTestedSnippet: TestedSnippet,
                                        allTestedSnippets: Snippet[],
                                        messages: OpenAIMessage[],
                                        initalLexer: string | undefined,
                                        initalParser: string | undefined,
                                        includeErrors: boolean = false,
                                        ) {
    const tempSolution: Grammar = {
        lexerSource: initalLexer ?? 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE (make it as general as possible as the language is more complex than this snippet)\n',
        parserSource: initalParser ?? 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
    };

    constructPrompt(tempSolution, firstNonWorkingTestedSnippet, allTestedSnippets, messages, false, includeErrors);
    const completion = await makeCompletionRequest(openaiEnv, messages, openaiEnv.model, undefined, 60*5);
    return completion.unwrap();
}