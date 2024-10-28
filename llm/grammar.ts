import OpenAI from "openai";
import { type OpenAIEnv } from "./utils";
import { TimeoutError, timeout } from 'promise-timeout';
import { grammarGenerationSystemMessage } from "./prompts";
import type { ANTLRError } from "../syntactic/ErrorListener";

export type Grammar = {
    lexerSource: string;
    parserSource: string;
};

export type GrammarWithMessageHistory = {
    grammar: Grammar;
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[];
};

export type MaybeGrammarWithHistory = {
    grammar?: Grammar;
    error?: string;
    completion?: string;
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[];
};

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

function constructPrompt(currentIntermediateSolution: Grammar | undefined, 
    codeSnippet: string, 
    errors: ANTLRError[]
): OpenAI.Chat.Completions.ChatCompletionMessageParam[] {
    const messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = []

    if (currentIntermediateSolution === undefined) {
        currentIntermediateSolution = {
            lexerSource: 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE\n',
            parserSource: 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
        };
    }

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
Write/fix the ANTLR4 lexer and parser grammars for the following code snippet:
\`\`\`
${codeSnippet}
\`\`\`
Start by shortly thinking step-by-step.`
    });
    return messages;
}

function parseCompletionToGrammar(completion: string | null): MaybeGrammarWithHistory {
    if (completion === null) {
        return {
            error: 'No completion provided',
            messages: []
        };
    }
    // Find all ```antlr blocks
    const antlrBlocks = completion.match(/```antlr\n([\s\S]*?)```/g);
    if (antlrBlocks === null) {
        return {
            error: 'No Antlr blocks found in completion',
            completion: completion,
            messages: []
        };
    }

    // Identify the lexer and parser blocks
    const lexerBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('lexer grammar'));
    const parserBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('parser grammar'));
    if (lexerBlock === undefined || parserBlock === undefined) {
        return {
            error: 'No lexer or parser block found in completion',
            completion: completion,
            messages: []
        };
    }

    // Extract the lexer and parser to just the code without the Antlr block
    const lexerSource = lexerBlock.replace('```antlr\n', '').replace('```', '');
    const parserSource = parserBlock.replace('```antlr\n', '').replace('```', '');

    return {
        grammar: {
            lexerSource,
            parserSource,
        },
        completion: completion,
        messages: []
    };
}

async function makeCompletionRequest(
    openaiEnv: OpenAIEnv,
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[],
    model: string,
    debug: undefined | 'input' | 'output' | 'both' = undefined,
    timeoutSeconds: number = 20,
): Promise<MaybeGrammarWithHistory> {
    //const models = ["ai21/jamba-1-5-mini", "google/gemini-flash-1.5"];
    //const randomModel = models[Math.floor(Math.random() * models.length)];
    try {
        const openai = new OpenAI({
            baseURL: openaiEnv.baseUrl,
            apiKey: openaiEnv.apiKey,
        });
        console.log(`LLM Call - Using model: ${model}, num_messages: ${messages.length}`);
        if (messages.length > 4) {
            //console.log(messages);
            console.log("LARGE MESSAGES");
        }
        const completionBody: OpenAI.Chat.Completions.ChatCompletionCreateParamsNonStreaming = {
            model: model,
            messages: messages,
            max_tokens: 4096,
            temperature: 0.8,
        };

        const completionPromise = openai.chat.completions.create(completionBody); // { body: { ...completionBody, provider: { order: ['Hyperbolic'] } } }

        // Wrap the completion promise with a timeout
        const completion = await timeout(completionPromise, timeoutSeconds * 1000);
        const content = completion.choices[0].message.content;
        
        if (debug === 'input' || debug === 'both') console.log(messages);
        if (debug === 'output' || debug === 'both') console.log(content);
        
        // Create new messages array with assistant's response
        const updatedMessages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] = [{
            role: 'assistant',
            content: content
        }];
        
        const result = parseCompletionToGrammar(content);
        result.messages = [...messages, ...updatedMessages];
        return result;
    } catch (error) {
        if (error instanceof TimeoutError) {
            return {
                error: `Request timed out after ${timeoutSeconds} seconds (model: ${model})`,
                messages: messages
            };
        }
        console.error(error);
        return {
            error: `API request failed: ${error instanceof Error ? error.message : String(error)}`,
            messages: messages
        };
    }
}

export async function generateCandidateSolutions(
    openaiEnv: OpenAIEnv,
    currentIntermediateSolution: Grammar | undefined,
    codeSnippet: string,
    errors: ANTLRError[] = [],
    n = 2
): Promise<GrammarWithMessageHistory[]> { 

    const messages = constructPrompt(currentIntermediateSolution, codeSnippet, errors);

    // Create array of n identical requests
    const requests = Array(n).fill(null).map(() => 
        makeCompletionRequest(openaiEnv, messages, openaiEnv.model)
    );

    // Execute all requests in parallel
    const results = await Promise.all(requests);

    // Extract the grammars that were parsed successfully
    const successfulGrammars = results.filter(result => result.error === undefined && result.grammar !== undefined);
    
    // Log bad grammars
    const badGrammars = results.filter(result => result.error !== undefined);
    if (badGrammars.length > 0) {
        console.error('Bad grammar generations:');
        badGrammars.forEach(result => {
            console.error(result.error);
            if (result.completion) {
                console.log(result.completion);
            }
        });
    }

    if (successfulGrammars.length === 0) {
        throw new Error('No successful grammars found');
    }

    // Convert successful results to GrammarWithMessageHistory
    return successfulGrammars.map(result => ({
        grammar: result.grammar!, // ! is fine since we filtered out the ones that didn't have a grammar
        messages: result.messages
    }));
}

export async function repairCandidateSolution(
    openaiEnv: OpenAIEnv,
    candidateSolution: GrammarWithMessageHistory,
    newErrors: ANTLRError[]
) {
    if (newErrors.length === 0) {
        throw new Error('No new errors provided');
    }
    const messages = candidateSolution.messages;
    messages.push({
        role: 'user',
        content: `I got some errors:
<Errors>
${newErrors.map(error => errorToString(error)).join('\n')}
</Errors>
Repair the grammar to fix the errors (same output format as before).`
    });
    const repairedGrammar = await makeCompletionRequest(openaiEnv, messages, openaiEnv.model);
    return {
        grammar: repairedGrammar.grammar,
        messages: repairedGrammar.messages
    };
}

export async function generateInitalGuess(openaiEnv: OpenAIEnv, snippets: string[]) {
    const combinedSnippets = snippets.join('\nNext snippet:\n');
    const tempSolution: Grammar = {
        lexerSource: 'lexer grammar MyLexer;\n\n// WRITE LEXER RULES HERE\n',
        parserSource: 'parser grammar MyParser;\noptions { tokenVocab=SimpleLangLexer; }\n\n// WRITE PARSER RULES HERE, Start rule must be called "program"\n',
    };
    const messages = constructPrompt(tempSolution, combinedSnippets, []);
    const completion = await makeCompletionRequest(openaiEnv, messages, openaiEnv.model);
    return {
        grammar: completion.grammar,
        messages: completion.messages
    };
}