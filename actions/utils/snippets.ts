import path from "path";
import fs from 'fs';
import { calculateComplexity } from '../../heuristics/complexity';
import type { Snippet } from '../../llm/grammar';
import { findAllCodeFiles } from './io';

function loadFilesToSnippets(files: string[]): Snippet[] {
		return files.map(file => {
				const fileContent = fs.readFileSync(file, 'utf8');
				return {
						snippet: fileContent,
						fileName: path.basename(file),
						filePath: file
				};
		});
}
function sortByComplexity(snippets: Snippet[]): Snippet[] {
		// Sort snippets by complexity (ascending)
		return snippets.sort((a, b) => calculateComplexity(b.snippet) - calculateComplexity(a.snippet)).reverse();
}

export async function loadSnippetsByComplexity(directory: string, extension: string, recursive: boolean) {
		const files = findAllCodeFiles(directory, extension, recursive);
		const snippets = loadFilesToSnippets(files.map(file => path.join(directory, file)));
		if (snippets.length === 0) {
				console.error('No files found with the specified extension!');
				return;
		}

		const sortedSnippets = sortByComplexity(snippets);

		const lowestComplexity = sortedSnippets[0];
		const highestComplexity = sortedSnippets[sortedSnippets.length - 1];
		console.log(`Loaded ${files.length} files and sorted them by complexity: ${calculateComplexity(lowestComplexity.snippet)} to ${calculateComplexity(highestComplexity.snippet)}`);
		console.log(`Analysing the files in the following order: ${sortedSnippets.map(snippet => snippet.fileName).join(', ')}`);

		return sortedSnippets;
}