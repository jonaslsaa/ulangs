import fs from 'fs';
import path from 'path';
import { loadOpenAIEnvVars } from '../llm/utils';
import { loadSnippetsByComplexity } from './utils/snippets';
import { AdapterContext } from '../llm/adapterPipeline';
import { GetQuery } from '../rules/queries/mapping';

/**
 * Evaluate a Prolog adapter against all code files in a directory.
 * Prints per-file results and a final average score.
 */
export async function evaluateAdapter(
	adapterPath: string,
	lexerPath: string,
	parserPath: string,
	directory: string,
	extension: string,
	recursive: boolean
): Promise<void> {
	// Read adapter source
	const adapterSource = fs.readFileSync(adapterPath, 'utf8');

	// Load all code snippets
	const snippets = await loadSnippetsByComplexity(directory, extension, recursive);
	if (!snippets || snippets.length === 0) {
		console.error(`No files found in ${directory} with extension ${extension}`);
		process.exit(1);
	}

	// Setup context and query
	const openaiEnv = loadOpenAIEnvVars();
	const query = GetQuery('holotype');
	const ctx = new AdapterContext(lexerPath, parserPath, openaiEnv);

	// Evaluate in parallel
	const results = await Promise.all(
		snippets.map(async snippet => {
			try {
				const tested = await ctx.testAdapterOnSnippet({ source: adapterSource }, snippet, query, []);
				return { snippet, tested };
			} catch (e: any) {
				console.error(`Error testing adapter on ${snippet.fileName}:`, e.message || e);
				// Fallback to a failed test with score 0
				const fallback = {
					onQuery: query,
					withSnippet: snippet,
					usedAdapter: { source: adapterSource },
					completeProlog: '',
					adapterLineRange: { start: 0, end: 0 },
					success: false,
					LLMScore: 0,
					errors: [{ type: 'ERROR', message: e.message || String(e), file: snippet.filePath }]
				};
				return { snippet, tested: fallback };
			}
		})
	);

	// Report
	let sumScore = 0;
	results.forEach(({ snippet, tested }) => {
		const score = tested.LLMScore ?? 0;
		sumScore += score;
		console.log(`${snippet.fileName}: ${score}/100 ${tested.success ? '✓' : '✗'}`);
	});
	const avg = (sumScore / results.length).toFixed(2);
	const successfulSnippets = results.filter(r => r.tested.success).length;
	console.log(`\nSuccessfully applied adapter to ${successfulSnippets}/${snippets.length} snippets`);
	console.log(`Final total score: ${sumScore}`);
	console.log(`Final average adapter score: ${avg}/100 over ${results.length} files.`);
}