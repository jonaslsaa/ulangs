import OpenAI from "openai";
import { type OpenAIEnv } from "./utils";
import { grammarGenerationSystemMessage } from "./prompts";

export type Grammar = {
    lexerSource: string;
    parserSource: string;
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
        content: `You are a programming language expert. Here is the current ANTLR4 solution:
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
Start by explaining what you will do at a high-level.`
    });
    console.log(messages);
    return messages;
}

type MaybeGrammar = {
    grammar?: Grammar;
    error?: string;
    completion?: string;
};

function parseCompletionToGrammar(completion: string | null): MaybeGrammar {
    if (completion === null) {
        return {
            error: 'No completion provided',
        };
    }
    // Find all ```antlr blocks
    const antlrBlocks = completion.match(/```antlr\n([\s\S]*?)```/g);
    if (antlrBlocks === null) {
        return {
            error: 'No Antlr blocks found in completion',
            completion: completion,
        };
    }

    // Identify the lexer and parser blocks
    const lexerBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('lexer grammar'));
    const parserBlock = antlrBlocks.find(block => block.trim().substring(0, 64).includes('parser grammar'));
    if (lexerBlock === undefined || parserBlock === undefined) {
        return {
            error: 'No lexer or parser block found in completion',
            completion: completion,
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
    };
}

async function makeCompletionRequest(
    openai: OpenAI,
    messages: OpenAI.Chat.Completions.ChatCompletionMessageParam[],
    model: string
): Promise<MaybeGrammar> {
    try {
        const completion = await openai.chat.completions.create({
            model: model,
            messages: messages,
            max_tokens: 4096,
            temperature: 0.7,
        });

        return parseCompletionToGrammar(completion.choices[0].message.content);
    } catch (error) {
        return {
            error: `API request failed: ${error instanceof Error ? error.message : String(error)}`,
        };
    }
}

export async function generateCandidateSolutions(
    openaiEnv: OpenAIEnv,
    currentIntermediateSolution: Grammar | undefined,
    codeSnippet: string,
    errors: string[] = [],
    errorUnderCompilationOfANTLRFiles: boolean = false,
    n = 2
) { 
    const openai = new OpenAI({
        baseURL: openaiEnv.baseUrl,
        apiKey: openaiEnv.apiKey,
    });

    const messages = constructPrompt(currentIntermediateSolution, codeSnippet, errors, errorUnderCompilationOfANTLRFiles);

    // Create array of n identical requests
    const requests = Array(n).fill(null).map(() => 
        makeCompletionRequest(openai, messages, openaiEnv.model)
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

    return successfulGrammars.map(result => result.grammar!);
}