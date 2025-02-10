import type { Generator, Verifier } from "./autoCreationLoop";
import type { OpenAIEnv, OpenAIMessage } from "../llm/utils";
import type { Snippet } from './grammar';

type Adapter = {
	source: string;
}

type AdapterError = {
	type: 'PROLOG' | 'SCHEMA' | 'JUDGE';
	message: string;
	file?: string;
	line?: number;
	column?: number;
}

type TestedSnippetWithAdapter = {
	onSnippet: Snippet;
	usedAdapter: Adapter;
	errors?: AdapterError[];
	success: boolean;
}

export class AdapterGenerator implements Generator<Adapter, Snippet, TestedSnippetWithAdapter> {
	openaiEnv: OpenAIEnv;
	messages: OpenAIMessage[];
	snippets: Snippet[];

	constructor(openaiEnv: OpenAIEnv, messages: OpenAIMessage[]) {
		this.openaiEnv = openaiEnv;
		this.messages = messages;
		this.snippets = [];
	}

	async generateInitialSolution(examples: Snippet[]): Promise<Adapter> {
		this.snippets = examples;
		return await buildFirstIntermediateSolution(
			this.openaiEnv,
			undefined,
			undefined,
			this.messages,
			examples
		);
	}

	async repairSolution(oldSolution: Adapter, failingExamples: Snippet[], failingResults: TestedSnippetWithAdapter[]): Promise<Adapter> {
		// Use the first failing snippet in the array as a starting point.
		const repairResults = await repairAdapter(
			this.openaiEnv,
			this.messages,
			failingResults,
			failingExamples[0],
			this.snippets
		);
		return repairResults[0].usedAdapter;
	}
}

export class AdapterVerifier implements Verifier<Adapter, Snippet, TestedSnippetWithAdapter> {
	async verify(solution: Adapter, example: Snippet): Promise<TestedSnippetWithAdapter> {
		// Use stop-on-first-failure style for verifying a single example.
		const results = await testGrammarOnMany(solution, example, []);
		return results[0];
	}
}