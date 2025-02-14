import type { Generator, Verifier } from "./autoCreationLoop";
import { midpoint, type OpenAIEnv, type OpenAIMessage } from "../llm/utils";
import type { Snippet } from './grammar';
import fs from 'fs';
import assert from 'assert';
import { getApplicableTreeClauses, getQueryClauses } from '../semantic/prolog';
import type { Parser, ParserRuleContext } from 'antlr4';
import { Err, Ok, type Result } from '../result';
import { checkGrammar } from '../syntactic/check-grammar';
import { createParserFromGrammar } from '../syntactic/context-free-parser';
import { init } from 'tiktoken/init';
import { executePrologQuery } from '../actions/query';
import tmp from 'tmp';
import { DefinitionQueryResultSchema } from '../rules/queries/schemas/definitions';
import { z, ZodSchema } from 'zod';
import { OpenAI } from 'openai';
import { zodResponseFormat } from "openai/helpers/zod";
import { adapterGenerationMessage, adapterScoringMessage } from './prompts';
import type { Query } from '../rules/queries/mapping';
import zodToJsonSchema from 'zod-to-json-schema';
import { Stats } from '../actions/utils';
import { compileANTLRFiles } from '../syntactic/build';
import path from 'path';
import { _createWorkingDirectory } from '../actions/utils/io';

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

type TestedAdapter = {
	onQuery: Query;
	withSnippet: Snippet;
	usedAdapter: Adapter;
	errors?: AdapterError[];
	success: boolean;
	LLMScore?: number;
}

type ParseableTree = {
	tree: ParserRuleContext;
	parser: Parser;
}

export class AdapterContext {
	openai: OpenAI;
	modelName: string;
	lexerPath: string;
	parserPath: string;
	_snippetToTreeCache: Map<string, ParseableTree>;

	MINUMUM_JUDGE_SCORE = 60;

	constructor(lexerPath: string, parserPath: string, openAIEnv: OpenAIEnv) {
		this.lexerPath = lexerPath;
		this.parserPath = parserPath;
		this.openai = new OpenAI({
			baseURL: openAIEnv.baseUrl,
			apiKey: openAIEnv.apiKey,
		});
		this.modelName = openAIEnv.model;

		this._snippetToTreeCache = new Map<string, ParseableTree>();
	}

	async snippetToTree(
		lexerPath: string,
		parserPath: string,
		snippet: Snippet
	): Promise<Result<ParseableTree>> {
		if (this._snippetToTreeCache.has(snippet.filePath)) {
			return Ok(this._snippetToTreeCache.get(snippet.filePath)!);
		}

		// First, check grammar validity as before.
		const errors = await checkGrammar(lexerPath, parserPath, snippet.filePath);
		if (errors.length > 0) {
			return Err(
				"Grammar validation failed: " +
				errors.map(error => error.message).join("\n")
			);
		}

		// Create a temporary working directory for adapter compilation.
		const tempDir = _createWorkingDirectory("adapter");
		const tempLexerPath = path.join(tempDir, "MyLexer.g4");
		const tempParserPath = path.join(tempDir, "MyParser.g4");

		// Copy the original .g4 files to the temporary directory.
		fs.copyFileSync(lexerPath, tempLexerPath);
		fs.copyFileSync(parserPath, tempParserPath);

		// Compile the copied .g4 files; this should generate MyLexer.ts and MyParser.ts.
		const compilationErrors = await compileANTLRFiles(tempDir);
		if (compilationErrors.length > 0) {
			return Err(
				"Compilation failed: " +
				compilationErrors.map(e => e.message).join("\n")
			);
		}

		// Use the compiled files to create the parser.
		const { parser, errorListener } = await createParserFromGrammar(
			snippet.snippet,
			tempLexerPath,
			tempParserPath
		);
		const tree = parser.program();

		// Cache and return the result.
		this._snippetToTreeCache.set(snippet.filePath, { tree, parser });
		const parseableTree: ParseableTree = {
			tree,
			parser,
		};
		return Ok(parseableTree);
	}

	async createFullQuery(
		adapterSource: string | undefined,
		snippet: Snippet,
		lexerPath: string,
		parserPath: string,
		mainQueryPath: string,
	): Promise<string> {
		const { tree, parser } = await (await this.snippetToTree(lexerPath, parserPath, snippet)).unwrap();

		const treeClauses = getApplicableTreeClauses(tree, parser, adapterSource, true);
		const queryClauses = getQueryClauses(mainQueryPath);
		const clauses = [...treeClauses, ...queryClauses];

		return clauses.join('\n');
	}

	async scoreAdapter(snippet: Snippet, adapterOutput: string) {
		const ScoringSchema = z.object({
			reasons: z.array(z.string()),
			score: z.number(),
		});

		const completion = await this.openai.beta.chat.completions.parse({
			model: this.modelName, // TODO: make this configurable
			messages: [
				{ role: "user", content: adapterScoringMessage(snippet.snippet, adapterOutput) },
			],
			response_format: zodResponseFormat(ScoringSchema, "score"),
		});

		const scoring = completion.choices[0].message.parsed;
		if (!scoring) {
			throw new Error("Failed to parse score: " + completion.choices[0].message.content);
		}
		return scoring;
	}

	async testAdapterOnSnippet(adapter: Adapter, snippet: Snippet, query: Query): Promise<TestedAdapter> {
		const testedAdapter: TestedAdapter = {
			onQuery: query,
			withSnippet: snippet,
			usedAdapter: adapter,
			errors: [],
			success: false,
		};

		// Try to run the with swi-prolog
		const { tree, parser } = await (await this.snippetToTree(this.lexerPath, this.parserPath, snippet)).unwrap();
		const fullProlog = await this.createFullQuery(adapter.source, snippet, this.lexerPath, this.parserPath, query.path);

		// Create temporary file with the clauses
		const tempFilePath = tmp.fileSync();
		fs.writeFileSync(tempFilePath.name, fullProlog);

		// Run the query
		const queryResult = executePrologQuery(tempFilePath.name);

		// Handle results
		if (queryResult.errors.length > 0) {
			queryResult.errors.forEach(error => testedAdapter.errors?.push({
				type: 'PROLOG',
				message: error,
				file: snippet.filePath,
				line: undefined,
				column: undefined,
			}));
			return testedAdapter;
		}

		// Next, let's check we actually got some output
		if (!queryResult.output || queryResult.output.trim() === '') {
			testedAdapter.errors?.push({
				type: 'PROLOG',
				message: 'Prolog returned empty result',
				file: snippet.filePath,
				line: undefined,
				column: undefined,
			});
			return testedAdapter;
		}

		// Now, let's check the output is valid JSON
		try {
			JSON.parse(queryResult.output);
		} catch (error) {
			testedAdapter.errors?.push({
				type: 'PROLOG',
				message: 'Prolog returned invalid JSON',
				file: snippet.filePath,
				line: undefined,
				column: undefined,
			});
			return testedAdapter;
		}

		// Now, let's check if output matches our schema
		const output = queryResult.output;
		const schema = DefinitionQueryResultSchema.safeParse(JSON.parse(output));
		if (!schema.success) {
			testedAdapter.errors?.push({
				type: 'SCHEMA',
				message: 'The prolog result did not match the expected schema: ' + schema.error.message,
				file: snippet.filePath,
				line: undefined,
				column: undefined,
			});
			// If we got any issues, let's add a note about the JSON schema
			if (schema.error.issues.length > 0) {
				testedAdapter.errors?.push({
					type: 'SCHEMA',
					message: 'JSON schema MUST be valid: ' + JSON.stringify(zodToJsonSchema(DefinitionQueryResultSchema), null, 2),
					file: snippet.filePath,
					line: undefined,
					column: undefined,
				});
			}
			return testedAdapter;
		}

		// Finally, let's check if the output matches the expected definition
		const scoring = await this.scoreAdapter(snippet, queryResult.output);
		scoring.reasons.forEach(reason => testedAdapter.errors?.push({
			type: 'JUDGE',
			message: reason,
			file: snippet.filePath,
			line: undefined,
			column: undefined,
		}));
		console.log("Scoring: ");
		console.log(scoring);
		testedAdapter.LLMScore = scoring.score;

		if (scoring.score < this.MINUMUM_JUDGE_SCORE) {
			return testedAdapter;
		}

		testedAdapter.success = true;
		return testedAdapter;
	}

	AdapterErrorsToString(adapterErrors?: AdapterError[]): string {
		if (!adapterErrors || adapterErrors.length === 0) {
			return '';
		}
		let prompt = 'I got the following errors:';
	
		// Map error types to an array of messages
		const errors = new Map<AdapterError['type'], string[]>();
		for (const { type, message } of adapterErrors) {
			const messages = errors.get(type) ?? [];
			messages.push(message);
			errors.set(type, messages);
		}
	
		// Build the string output
		for (const [type, messages] of errors.entries()) {
			prompt += `\n<${type}Errors>\n${messages.join('\n')}\n</${type}Errors>`;
		}
	
		return prompt;
	}

	parseStringToAdapterSource(completion: string): Result<string> {
		const adapterSource = completion.match(/<Adapter>\n([\s\S]*?)\n<\/Adapter>/);
		if (!adapterSource) return Err('No adapter source found in completion');
		return Ok(adapterSource[1]);
	}

	_midpoint(array: any[]): any {
		if (array.length === 0) return undefined;
		if (array.length === 1) return array[0];
		if (array.length === 2) return array[0];
		return array[Math.floor(array.length / 2)];
	}

	async buildFirstIntermediateSolution(
		initialAdapter: string | undefined,
		representativeSnippet: Snippet,
		queries: Query[],
		lexerPath: string,
		parserPath: string,
	): Promise<{ adapter: Adapter, messages: OpenAIMessage[] }> {
		const messages: OpenAIMessage[] = [];

		const query = queries[0]; // TODO: buildFirstIntermediateSolution should create a generic adapter/solution that solves all queries

		// Create draft query (this will either have no or just a template adapter)
		const draftQuery = await this.createFullQuery(
			initialAdapter,
			representativeSnippet,
			lexerPath,
			parserPath,
			query.path
		);

		let prompt: string = adapterGenerationMessage;
		prompt += "\n<SourceCode>\n" + representativeSnippet + "\n</SourceCode>";
		prompt += "\n<FullProlog>\n" + draftQuery + "\n</FullProlog>";
		// NOTE: we could also hint the JSON Schema but it should be implicit from the main query, we can rather hint it if it fails

		// Do inital test if an initial adapter was provided
		if (initialAdapter) {
			const adapter: Adapter = {
				source: initialAdapter,
			};
			const testedInitialAdapter = await this.testAdapterOnSnippet(adapter, representativeSnippet, query);
			if (testedInitialAdapter.success) return { adapter, messages }; // Return early if the initial adapter is valid

			// Add errors to the prompt
			prompt += this.AdapterErrorsToString(testedInitialAdapter.errors);
		}

		// Ask for a draft solution
		prompt += "\nTask: Develop and output a new adapter. You MUST output in XML tag, in following format:\n<Adapter>\n// WRITEABLE AREA\n...\n</Adapter>";
		messages.push({
			role: 'user',
			content: prompt,
		});

		Stats.addRequest();
		const completion = await this.openai.chat.completions.create({
			model: this.modelName,
			messages,
		});

		const content = completion.choices[0].message.content;
		if (content === null) throw new Error('No completion provided');

		// add content to messages
		messages.push({
			role: 'assistant',
			content: content,
		});

		Stats.addCompletedRequest(completion);

		// Extract the adapter from the completion
		const adapterSource = await this.parseStringToAdapterSource(content);
		const adapter = {
			source: adapterSource.unwrap(),
		}
		return { adapter, messages };
	}

	async repairAdapter(
		oldAdapter: Adapter,
		failingExamples: Query[],
		failingResults: TestedAdapter[],
		messages: OpenAIMessage[]
	): Promise<Adapter> {
		// Build a user prompt that includes the old adapter, failing snippet(s), and errors.
		let prompt: string = '';

		// Add adapter to prompt
		//let prompt = `<Adapter>\n${oldAdapter.source}\n</Adapter>\n\n`;

		// Add failing snippets to prompt with errors
		for (const tested of failingResults) {
			if (!tested.success) {
				// prompt += `<SourceCode>\n${tested.withSnippet.snippet}\n</SourceCode>\n`;
				if (tested.errors && tested.errors.length > 0) {
					prompt += this.AdapterErrorsToString(tested.errors);
				}
			}
		}

		prompt += `\n\nPlease fix the <Adapter> so the queries succeed without breaking previously passing logic. 
Output exactly one <Adapter> block.`;

		// Add prompt to messages
		messages.push({
			role: 'user',
			content: prompt,
		});

		// Make a single LLM call.
		let newAdapterSource: string | undefined;
		try {
			Stats.addRequest();
			const completion = await this.openai.chat.completions.create({
				model: this.modelName,
				messages: messages
			});
			console.log(JSON.stringify(messages, null, 2));
			const content = completion.choices[0].message.content;
			if (content === null) throw new Error('No completion provided');

			Stats.addCompletedRequest(completion);
			const adapterParse = this.parseStringToAdapterSource(content);
			if (adapterParse.isErr()) {
				console.warn("Failed to parse new adapter from LLM output. Returning old adapter.");
				return oldAdapter; // Let autoCreationLoop decide next steps
			}
			newAdapterSource = adapterParse.unwrap();

			// Add content to messages
			messages.push({
				role: 'assistant',
				content: content,
			});

		} catch (err) {
			console.error("LLM request failed:", err);
			return oldAdapter; // fallback
		}

		// If all went well, return the newly generated adapter.
		return { source: newAdapterSource };
	}
}

export class AdapterGenerator implements Generator<Adapter, Query, TestedAdapter> {
	openaiEnv: OpenAIEnv;
	messages: OpenAIMessage[];
	snippets: Snippet[];
	queries: Query[];
	adapterContext: AdapterContext;

	lexerPath: string;
	parserPath: string;
	initialAdapter: string | undefined;

	constructor(openaiEnv: OpenAIEnv, messages: OpenAIMessage[], lexerPath: string, parserPath: string, initialAdapter: string | undefined, snippets: Snippet[]) {
		this.openaiEnv = openaiEnv;
		this.messages = messages;
		this.snippets = snippets;
		this.queries = [];
		this.lexerPath = lexerPath;
		this.parserPath = parserPath;
		this.initialAdapter = initialAdapter;

		this.adapterContext = new AdapterContext(lexerPath, parserPath, openaiEnv);
	}

	async generateInitialSolution(examples: Query[]): Promise<Adapter> {
		if (this.messages.length > 0) throw new Error('Cannot generate initial solution after messages have been set');
		this.queries = examples;

		// Choose a snippet that represents the language well
		const representativeSnippet = midpoint(this.snippets); // TODO: this isn't that good of a heuristic
		assert(representativeSnippet);

		const { adapter, messages } = await this.adapterContext.buildFirstIntermediateSolution(this.initialAdapter, representativeSnippet, this.queries, this.lexerPath, this.parserPath);
		this.messages = messages;
		return adapter;
	}

	/**
	 * Called by the autoCreationLoop when a solution fails some queries. We delegate
	 * to AdapterContext#repairAdapter, which does the multi-step LLM repair loop.
	 */
	async repairSolution(
		oldSolution: Adapter,
		failingExamples: Query[],
		failingResults: TestedAdapter[]
	): Promise<Adapter> {
		return await this.adapterContext.repairAdapter(oldSolution, failingExamples, failingResults, this.messages);
	}
}

export class AdapterVerifier implements Verifier<Adapter, Query, TestedAdapter> {
	snippets: Snippet[];
	adapterContext: AdapterContext;

	constructor(adapterContext: AdapterContext, snippets: Snippet[]) {
		this.adapterContext = adapterContext;
		this.snippets = snippets;
	}

	async verify(solution: Adapter, example: Query): Promise<TestedAdapter> {
		// Use stop-on-first-failure style for verifying a single example.
		const representativeSnippet = midpoint(this.snippets);
		assert(representativeSnippet);
		return await this.adapterContext.testAdapterOnSnippet(solution, representativeSnippet, example);
	}
}