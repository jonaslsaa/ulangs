/**
 * grammarPipeline.ts
 *
 * Domain-specific adapter implementations for ANTLR grammar generation.
 * These classes plug into the generic interfaces from autoCreationLoop.ts.
 */

/**
 * grammarPipeline.ts
 *
 * Domain-specific adapter implementations for ANTLR grammar generation.
 * These classes plug into the generic interfaces from autoCreationLoop.ts.
 */
import type { Generator, Verifier } from "./autoCreationLoop";
import type { Grammar, Snippet, TestedSnippet } from "./grammar"; // Adjust path as needed.
import { buildFirstIntermediateSolution, repairGrammar, testGrammarOnMany } from "../actions/inferGrammar"; // Adjust path as needed.
import type { OpenAIEnv, OpenAIMessage } from "../llm/utils";
import fs from 'fs';

/**
 * GrammarGenerator
 *
 * Uses existing functions to generate an initial grammar and repair it via LLM-based suggestions.
 */
export class GrammarGenerator implements Generator<Grammar, Snippet, TestedSnippet> {
  openaiEnv: OpenAIEnv;
  messages: OpenAIMessage[];
  snippets: Snippet[];

  initialLexerPath?: string;
  initialParserPath?: string;

  constructor(openaiEnv: OpenAIEnv, messages: OpenAIMessage[], initialLexerPath?: string, initialParserPath?: string) {
    this.openaiEnv = openaiEnv;
    this.messages = messages;
    this.snippets = [];
    this.initialLexerPath = initialLexerPath;
    this.initialParserPath = initialParserPath;
  }

  async generateInitialSolution(examples: Snippet[]): Promise<Grammar> {
    this.snippets = examples;
    
    // Load initial grammar files if specified
    let initialLexer: string | undefined;
    let initialParser: string | undefined;
    
    if (this.initialLexerPath) {
      initialLexer = fs.readFileSync(this.initialLexerPath, 'utf8');
    }
    if (this.initialParserPath) {
      initialParser = fs.readFileSync(this.initialParserPath, 'utf8');
    }

    return await buildFirstIntermediateSolution(
      this.openaiEnv,
      initialLexer,
      initialParser,
      this.messages,
      examples
    );
  }

  async repairSolution(oldSolution: Grammar, failingExamples: Snippet[], failingResults: TestedSnippet[], lastExampleWasNotSolved: boolean): Promise<Grammar> {
    // Use the first failing snippet in the array as a starting point.
    const repairResults = await repairGrammar(
      this.openaiEnv,
      this.messages,
      failingResults,
      failingExamples[0],
      this.snippets
    );
    return repairResults[0].usedGrammar;
  }
}

/**
 * GrammarVerifier
 *
 * Tests an ANTLR grammar on a given code snippet.
 */
export class GrammarVerifier implements Verifier<Grammar, Snippet, TestedSnippet> {
  async verify(solution: Grammar, example: Snippet): Promise<TestedSnippet> {
    // Use stop-on-first-failure style for verifying a single example.
    const results = await testGrammarOnMany(solution, example, []);
    return results[0];
  }
}
