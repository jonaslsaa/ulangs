import OpenAI from "openai";
import { type OpenAIEnv } from "./utils";
import { TimeoutError, timeout } from 'promise-timeout';
import { grammarGenerationSystemMessage } from "./prompts";

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

function constructPrompt(currentIntermediateSolution: Grammar | undefined, 
    codeSnippet: string, 
    errors: string[] = [],
    errorUnderCompilationOfANTLRFiles: boolean = false
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

    // Add user message
    let errorMessage = '';
    if (errors.length > 0) {
        // Limit amount of errors to 10
        const maxErrors = 10;
        if (errors.length > maxErrors) {
            errors = errors.slice(0, maxErrors);
            errors.push(`...and ${errors.length - maxErrors} more errors...`);
        }
        errorMessage = `\n\nThe following errors were found in the current solution:\n<Errors>\n${errors.join('\n')}\n</Errors>`;
        if (errorUnderCompilationOfANTLRFiles) {
            errorMessage += `\nThe error occurred under compilation of ANTLR4 files.`;
        } else {
            errorMessage += `\nThe error occurred during the parsing of the code snippet.`;
        }
    }
    messages.push({
        role: 'user',
        content: `Here is the current ANTLR4 solution:
<LexerGrammar>
\`\`\`antlr
${currentIntermediateSolution.lexerSource}
\`\`\`
</LexerGrammar>
<ParserGrammar>
\`\`\`antlr
${currentIntermediateSolution.parserSource}
\`\`\`
</ParserGrammar>
${errorMessage}
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
    timeoutSeconds: number = 30
): Promise<MaybeGrammarWithHistory> {
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
                error: `Request timed out after ${timeoutSeconds} seconds`,
                messages: messages
            };
        }
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
    errors: string[] = [],
    errorUnderCompilationOfANTLRFiles: boolean = false,
    n = 8
): Promise<GrammarWithMessageHistory[]> { 

    const messages = constructPrompt(currentIntermediateSolution, codeSnippet, errors, errorUnderCompilationOfANTLRFiles);

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
        grammar: result.grammar!,
        messages: result.messages
    }));
}

export async function repairCandidateSolution(
    openaiEnv: OpenAIEnv,
    candidateSolution: GrammarWithMessageHistory,
    newErrors: string[]
) {

    const messages = candidateSolution.messages;
    messages.push({
        role: 'user',
        content: `I got the following errors:
        <Errors>
        ${newErrors.join('\n')}
        </Errors>
        Repair the grammar to fix the errors (same output format as before).`
    });
    const repairedGrammar = await makeCompletionRequest(openaiEnv, messages, openaiEnv.model, 'both');
    return {
        grammar: repairedGrammar.grammar,
        messages: repairedGrammar.messages
    };
}