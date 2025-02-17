import type { Generator, Verifier } from "./autoCreationLoop";
import { midpoint, overlayErrorsOnCodeWithSnippetRange, type OpenAIEnv, type OpenAIMessage } from "../llm/utils";
import type { Snippet } from './grammar';
import fs from 'fs';
import assert from 'assert';
import { getApplicableTreeClauses, getQueryClauses } from '../semantic/prolog';
import type { Parser, ParserRuleContext } from 'antlr4';
import { Err, Ok, type Result } from '../result';
import { checkGrammar } from '../syntactic/check-grammar';
import { createParserFromGrammar } from '../syntactic/context-free-parser';
import { executePrologQuery } from '../actions/query';
import tmp from 'tmp';
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
	completeProlog: string;
	adapterLineRange: { start: number, end: number };
	output?: string;
	errors?: AdapterError[];
	success: boolean;
	LLMScore?: number;
}

type ParseableTree = {
	tree: ParserRuleContext;
	parser: Parser;
}

const ScoringSchema = z.object({
	errorsAndLimitations: z.array(z.string()),
	score: z.number(),
});

export class AdapterContext {
	openai: OpenAI;
	openaiEnv: OpenAIEnv;
	lexerPath: string;
	parserPath: string;
	_snippetToTreeCache: Map<string, ParseableTree>;
	_scoreAdapterCache: Map<string, z.infer<typeof ScoringSchema>>;

	MINUMUM_JUDGE_SCORE = 80;

	constructor(lexerPath: string, parserPath: string, openaiEnv: OpenAIEnv) {
		this.lexerPath = lexerPath;
		this.parserPath = parserPath;
		this.openai = new OpenAI({
			baseURL: openaiEnv.baseUrl,
			apiKey: openaiEnv.apiKey,
		});
		this.openaiEnv = openaiEnv;

		this._snippetToTreeCache = new Map<string, ParseableTree>();
		this._scoreAdapterCache = new Map<string, z.infer<typeof ScoringSchema>>();
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
		if (adapterSource && adapterSource.split('\n').length < 2) {
			throw new Error("Adapter source must contain at least two lines, got this: " + adapterSource);
		}
		const { tree, parser } = await (await this.snippetToTree(lexerPath, parserPath, snippet)).unwrap();

		const treeClauses = getApplicableTreeClauses(tree, parser, adapterSource, true);
		const queryClauses = getQueryClauses(mainQueryPath);
		const clauses = [...treeClauses, ...queryClauses];

		return clauses.join('\n');
	}

	async scoreAdapter(snippet: Snippet, adapterOutput: string) {
		const _key = snippet.snippet + '|' + adapterOutput;
		if (this._scoreAdapterCache.has(_key)) {
			console.log("Cache hit for adapter scoring.");
			return this._scoreAdapterCache.get(_key)!;
		}

		Stats.addRequest();
		const completion = await this.openai.beta.chat.completions.parse({
			model: this.openaiEnv.soModel,
			messages: [
				{ role: "user", content: adapterScoringMessage(snippet.snippet, adapterOutput) },
			],
			response_format: zodResponseFormat(ScoringSchema, "score"),
		});
		Stats.addCompletedRequest(completion);

		const scoring = completion.choices[0].message.parsed;
		if (!scoring) {
			throw new Error("Failed to parse score: " + completion.choices[0].message.content);
		}
		this._scoreAdapterCache.set(_key, scoring); // Cache the result
		return scoring;
	}

	_findAdapterLineRange(fullProlog: string, adapterSource: string): { start: number; end: number } | undefined {
		// Find the first exact occurrence of the adapterSource in the fullProlog.
		const idx = fullProlog.indexOf(adapterSource);
		if (idx === -1) {
			// If there's no exact match, return undefined.
			return undefined;
		}
	
		// Count how many lines occur in fullProlog before the match.
		// Using slice(0, idx) means all characters before the match.
		const before = fullProlog.slice(0, idx);
		const startLine = before.split('\n').length; // 1-based line indexing.
	
		// How many lines does the adapter source span?
		// This is simply how many line-breaks are in the adapterSource itself.
		const adapterLineCount = adapterSource.split('\n').length;
	
		// The end line is (startLine + adapterLineCount - 1).
		// -1 because if you start on line 5 and the adapter is 1 line long, it also ends on line 5.
		const endLine = startLine + adapterLineCount - 1;
	
		return { start: startLine, end: endLine };
	}


	async testAdapterOnSnippet(adapter: Adapter, snippet: Snippet, query: Query): Promise<TestedAdapter> {
		console.log("  - Testing adapter on snippet:", snippet.fileName);

		// Try to run the with swi-prolog
		const fullProlog = await this.createFullQuery(adapter.source, snippet, this.lexerPath, this.parserPath, query.path);

		// Find where the adapter is in the code
		const adapterLineRange = this._findAdapterLineRange(fullProlog, adapter.source);
		assert(adapterLineRange);

		const testedAdapter: TestedAdapter = {
			onQuery: query,
			withSnippet: snippet,
			usedAdapter: adapter,
			completeProlog: fullProlog,
			adapterLineRange,
			errors: [],
			success: false,
		};

		// Create temporary file with the clauses
		const tempFilePath = tmp.fileSync();
		fs.writeFileSync(tempFilePath.name, fullProlog);

		// Run the query
		const queryResult = executePrologQuery(tempFilePath.name);

		// Handle results
		if (queryResult.errors.length > 0) {
			queryResult.errors.forEach(error => testedAdapter.errors?.push({
				type: 'PROLOG',
				message: error.message,
				file: snippet.filePath,
				line: error.line,
				column: error.column,
			}));
			console.log("    - Failed to run query (0/5).");
			console.dir(queryResult.errors);
			console.log("Ran prolog:", tempFilePath.name, "and got:");
			console.log(queryResult.output);
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
			console.log("    - Empty result (1/5).");
			return testedAdapter;
		}

		// add the output to the testedAdapter
		testedAdapter.output = queryResult.output;

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
			console.log("    - Invalid JSON (2/5).");
			return testedAdapter;
		}

		// Now, let's check if output matches our schema
		const output = queryResult.output;
		const schema = query.schema.safeParse(JSON.parse(output));
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
					message: 'JSON schema MUST be valid: ' + JSON.stringify(zodToJsonSchema(query.schema), null, 2),
					file: snippet.filePath,
					line: undefined,
					column: undefined,
				});
			}
			console.log("    - Invalid JSON schema (3/5).");
			return testedAdapter;
		}

		// Finally, let's check if the output matches the expected definition
		const scoring = await this.scoreAdapter(snippet, queryResult.output);
		scoring.errorsAndLimitations.forEach(reason => testedAdapter.errors?.push({
			type: 'JUDGE',
			message: reason,
			file: snippet.filePath,
			line: undefined,
			column: undefined,
		}));

		testedAdapter.LLMScore = scoring.score;

		if (scoring.score < this.MINUMUM_JUDGE_SCORE) {
			console.log("    - Score too low (4/5), reasons:");
			console.log(scoring.errorsAndLimitations);
			console.log("      - Score:", scoring.score);
			return testedAdapter;
		}
		console.log(scoring.errorsAndLimitations);
		console.log("    - Score:", scoring.score);

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
			prompt += `\n<${type}Errors>\n${messages.map(m => " - " + m).join('\n')}\n</${type}Errors>`;
		}

		return prompt;
	}

	parseStringToAdapterSource(completion: string): Result<string> {
		const adapterSource = completion.match(/<Adapter>\n([\s\S]*?)\n<\/Adapter>/);
		if (!adapterSource) return Err('No adapter source found in completion');
		return Ok(adapterSource[1]);
	}

	async buildFirstIntermediateSolution(
		initialAdapterPath: string | undefined,
		holotypeQuery: Query,
		examples: Snippet[],
		lexerPath: string,
		parserPath: string,
	): Promise<{ adapter: Adapter, messages: OpenAIMessage[] }> {
		const messages: OpenAIMessage[] = [];

		// TODO: decide if we should have more than one representative snippet
		const representativeSnippet = midpoint(examples); // Let's just choose one
		assert(representativeSnippet);

		// Create draft query (this will either have no or just a template adapter)
		let initialAdapter: string | undefined;
		if (initialAdapterPath) {
			initialAdapter = fs.readFileSync(initialAdapterPath, 'utf8');
		}

		const draftQuery = await this.createFullQuery(
			initialAdapter,
			representativeSnippet,
			lexerPath,
			parserPath,
			holotypeQuery.path
		);

		let prompt: string = adapterGenerationMessage;
		prompt += "\n<FullProlog>\n" + draftQuery + "\n</FullProlog>";
		prompt += "\nThe auto-generated tree is generated from the following code:";
		prompt += "\n<SourceCode>\n" + representativeSnippet.snippet + "\n</SourceCode>";
		// NOTE: we could also hint the JSON Schema but it should be implicit from the main query, we can rather hint it if it fails

		// Do inital test if an initial adapter was provided
		if (initialAdapter) {
			const adapter: Adapter = {
				source: initialAdapter,
			};
			const testedInitialAdapter = await this.testAdapterOnSnippet(adapter, representativeSnippet, holotypeQuery);
			if (testedInitialAdapter.success) return { adapter, messages }; // Return early if the initial adapter is valid

			// Add initial adapter errors to the prompt
			prompt += '\n<CurrentAdapter>\n' + adapter.source + '\n</CurrentAdapter>\n';

			// Add errors to the prompt
			prompt += this.AdapterErrorsToString(testedInitialAdapter.errors);
		}

		// Ask for a draft solution
		prompt += "\nTask: Develop and output a new adapter. Make sure that the current main query will run with the adapter.";
		prompt += "\nYou MUST output in XML tag, in following format:\n<Adapter>\n% WRITEABLE AREA\n...\n</Adapter>";
		messages.push({
			role: 'user',
			content: prompt,
		});

		Stats.addRequest();
		const completion = await this.openai.chat.completions.create({
			model: this.openaiEnv.model,
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

	hasErrorsOfType(errors: AdapterError[], type: 'PROLOG' | 'SCHEMA' | 'JUDGE'): boolean {
		return errors.some(error => error.type === type);
	}

	_compressJSONIfPossible(json: string): string {
		try {
			return JSON.stringify(JSON.parse(json), null, 1);
		} catch (e) {
			return json;
		}
	}

	async repairAdapter(
		oldAdapter: Adapter,
		failingExamples: Snippet[],
		failingResults: TestedAdapter[],
		messages: OpenAIMessage[],
		lastExampleWasNotSolved: boolean
	): Promise<Adapter> {

		if (messages.length === 0) {
			throw new Error('No messages provided, this should not happen as the repairAdapter doesn\'t add context to the messages.');
		}

		// Build a user prompt that includes the old adapter, failing snippet(s), and errors.
		let prompt: string = '';

		if (lastExampleWasNotSolved) {
			prompt += "Hmm, this isn't working out. Let's try with some other snippets.\n";
		}

		// Add adapter to prompt
		// prompt = `<Adapter>\n${oldAdapter.source}\n</Adapter>\n\n`;

		// Add failing snippets to prompt with errors
		for (const tested of failingResults) {
			if (tested.errors && !tested.success) {
				// If we have JUDGE errors, add the source code as context
				if (this.hasErrorsOfType(tested.errors, 'JUDGE')) {
					prompt += `<SourceCode>\n${tested.withSnippet.snippet}\n</SourceCode>\n`;
				}

				// If we have schema or judge errors, add the prolog output as context
				if (this.hasErrorsOfType(tested.errors, 'SCHEMA') || this.hasErrorsOfType(tested.errors, 'JUDGE')) {
					assert(tested.output);
					const compressedOutput = this._compressJSONIfPossible(tested.output);
					prompt += `<PrologOutput>\n${compressedOutput}\n</PrologOutput>\n`;
				}

				// Add score if availablesource
				if (tested.LLMScore) {
					prompt += `\nI'd give this adapter a score of ${tested.LLMScore} out of 100.\n`;
				}

				// If we have PROLOG errors, add the prolog code as context with the errors in comments
				if (this.hasErrorsOfType(tested.errors, 'PROLOG')) {
					const errorsWithLines = tested.errors.filter(error => error.line !== undefined);
					if (errorsWithLines.length > 0) {
						assert(oldAdapter.source.split('\n').length > 2);
						const prologCodeWithErrorsOverlay = overlayErrorsOnCodeWithSnippetRange(tested.usedAdapter.source, errorsWithLines, '%', tested.adapterLineRange);
						prompt += '\nHere is the prolog i ran with the errors in % comments:\n';
						prompt += `<PrologCodeRanWithErrors>\n${prologCodeWithErrorsOverlay}\n</PrologCodeRanWithErrors>`;
					}
				}

				// Add all the errors to the prompt as well
				if (tested.errors.length > 0) {
					prompt += this.AdapterErrorsToString(tested.errors);
				}
			}
			if (!tested.success && (tested.errors || []).length === 0) {
				throw new Error('Unsucessful test result with no errors ???');
			}
		}

		prompt += `\n\nPlease fix the <Adapter> so the queries succeed without breaking previously passing logic.
Output exactly one <Adapter> block, then a concise <ChangesAndNotes> block with the changes and notes for the work so far`;

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
				model: this.openaiEnv.model,
				messages: messages
			});
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

			// DEBUG LOG
			console.log(JSON.stringify(messages, null, 2));

		} catch (err) {
			console.error("LLM request failed:", err);
			return oldAdapter; // fallback
		}

		// If all went well, return the newly generated adapter.
		return { source: newAdapterSource };
	}
}

export class AdapterGenerator implements Generator<Adapter, Snippet, TestedAdapter> {
	openaiEnv: OpenAIEnv;
	messages: OpenAIMessage[];
	holotypeQuery: Query;
	examples: Snippet[];
	adapterContext: AdapterContext;

	lexerPath: string;
	parserPath: string;
	initialAdapter: string | undefined;

	constructor(openaiEnv: OpenAIEnv, messages: OpenAIMessage[], lexerPath: string, parserPath: string, initialAdapter: string | undefined, query: Query) {
		this.openaiEnv = openaiEnv;
		this.messages = messages;
		this.holotypeQuery = query;
		this.examples = [];
		this.lexerPath = lexerPath;
		this.parserPath = parserPath;
		this.initialAdapter = initialAdapter;

		this.adapterContext = new AdapterContext(lexerPath, parserPath, openaiEnv);
	}

	async generateInitialSolution(examples: Snippet[]): Promise<Adapter> {
		if (this.messages.length > 0) throw new Error('Cannot generate initial solution after messages have been set');
		this.examples = examples;

		console.log("Using representative query:", this.holotypeQuery.path);

		const { adapter, messages } = await this.adapterContext.buildFirstIntermediateSolution(this.initialAdapter,
			this.holotypeQuery,
			this.examples,
			this.lexerPath,
			this.parserPath);
		this.messages = messages;
		return adapter;
	}

	/**
	 * Called by the autoCreationLoop when a solution fails some queries. We delegate
	 * to AdapterContext#repairAdapter, which does the multi-step LLM repair loop.
	 */
	async repairSolution(
		oldSolution: Adapter,
		failingExamples: Snippet[],
		failingResults: TestedAdapter[],
		lastExampleWasNotSolved: boolean
	): Promise<Adapter> {
		return await this.adapterContext.repairAdapter(oldSolution, failingExamples, failingResults, this.messages, lastExampleWasNotSolved);
	}
}

export class AdapterVerifier implements Verifier<Adapter, Snippet, TestedAdapter> {
	snippets: Snippet[];
	adapterContext: AdapterContext;
	holotypeQuery: Query;

	constructor(adapterContext: AdapterContext, snippets: Snippet[], holotypeQuery: Query) {
		this.adapterContext = adapterContext;
		this.snippets = snippets;
		this.holotypeQuery = holotypeQuery;
	}

	async verify(solution: Adapter, example: Snippet): Promise<TestedAdapter> {
		return await this.adapterContext.testAdapterOnSnippet(solution, example, this.holotypeQuery);
	}
}