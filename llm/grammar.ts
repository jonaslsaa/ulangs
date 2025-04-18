import OpenAI from "openai";
import { type OpenAIEnv, type OpenAIMessage, loadOpenAIEnvVars, overlayErrorsOnCode } from './utils';
import { TimeoutError, timeout } from 'promise-timeout';
import { grammarGenerationDeveloperMessage } from "./prompts";
import type { ANTLRError } from "../syntactic/ErrorListener";
import { Err, Ok, type Result } from "../result";
import { Stats } from '../actions/utils';

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
        const lexerErrorsWithLine = errors.filter(error => error.line !== undefined && error.type === 'LEXER' && error.source === 'BUILD');
        const parserErrorsWithLine = errors.filter(error => error.line !== undefined && error.type === 'PARSER' && error.source === 'BUILD');
        lexerSource = overlayErrorsOnCode(currentIntermediateSolution.lexerSource, lexerErrorsWithLine);
        parserSource = overlayErrorsOnCode(currentIntermediateSolution.parserSource, parserErrorsWithLine);

        const runtimeErrorsWithLine = errors.filter(error => error.line !== undefined && error.type === 'PARSER' && error.source === 'RUNTIME');

        const remainingErrors = errors.filter(error => !(lexerErrorsWithLine.includes(error) || parserErrorsWithLine.includes(error) || runtimeErrorsWithLine.includes(error)));

        // Overlay the runtime errors on the code snippet
        snippetWithErrors = overlayErrorsOnCode(codeSnippet, runtimeErrorsWithLine);

        // Put other error messages in its own block
        if (remainingErrors.length > 0) {
            otherErrorsBlock = `<OtherErrors>
    ${remainingErrors.map(error => `Under ${error.source}, in the ${error.type} the following error occurred: ${error.message}`).join('\n')}
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
    const lexerBlocks = antlrBlocks.filter(block => block.trim().substring(0, 64).includes('lexer grammar'));
    const parserBlocks = antlrBlocks.filter(block => block.trim().substring(0, 64).includes('parser grammar'));
    if (lexerBlocks.length === 0 || parserBlocks.length === 0) {
        return Err('No lexer or parser blocks found in completion');
    }
    // Choose the largest lexer and parser blocks
    const lexerBlock = lexerBlocks.sort((a, b) => b.length - a.length)[0];
    const parserBlock = parserBlocks.sort((a, b) => b.length - a.length)[0];

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
            store: true,
			metadata: {
				type: "synthesis",
                subType: "synthesis-and-repair",
			},
        };

        Stats.addRequest();
        const completionPromise = openai.chat.completions.create(completionBody); // { body: { ...completionBody, provider: { order: ['Hyperbolic'] } } }

        // Wrap the completion promise with a timeout
        const completion = await timeout(completionPromise, timeoutSeconds * 1000);
        const content = completion.choices[0].message.content;
        
        if (debug === 'input' || debug === 'both') console.log(messages);
        if (debug === 'output' || debug === 'both') console.log(content);
        
        const result = parseCompletionToGrammar(content);

        if (completion.usage) Stats.addCompletedRequest(completion);

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