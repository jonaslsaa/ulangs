/**
 * autoCreationLoop.ts
 *
 * Provides a domain-agnostic auto-creation loop that:
 *  - Generates an initial solution based on examples.
 *  - Verifies the solution.
 *  - Repairs it if needed.
 *
 * Supports both batch and incremental (per-snippet) modes,
 * checkpointing intermediate solutions, and an option to
 * repair using multiple failing examples.
 */

/** Generator interface: implement to generate or repair a solution. */
export interface Generator<Solution, Example, Result> {
	generateInitialSolution(examples: Example[]): Promise<Solution>;
	repairSolution(
		oldSolution: Solution,
		failingExamples: Example[],
		failingResults: Result[]
	): Promise<Solution>;
}

/** Verifier interface: implement to test a solution on a given example. */
export interface Verifier<Solution, Example, Result> {
	verify(solution: Solution, example: Example): Promise<Result>;
}

/** Candidate type: stores a solution, its test results, and a score. */
export interface Candidate<Solution, Example, Result> {
	solution: Solution;
	results: Array<{ example: Example; result: Result }>;
	score: number; // e.g., number of examples passing.
}

/** Options to configure the inference loop. */
export interface InferenceOptions {
	maxRetries?: number;
	stopOnFirstFailure?: boolean;
	incremental?: boolean; // Process examples one-by-one.
	repairAllFailingExamples?: boolean; // Pass all failing examples to repairSolution.
	messageCompressor?: (messages: any[]) => any[]; // (Optional) for LLM context management.
	checkpointHook?: (candidate: Candidate<any, any, any>) => void;
}

/**
 * evaluateSolution
 *
 * Evaluates the given solution on a set of examples using the provided verifier.
 */
async function evaluateSolution<Solution, Example, Result extends { success: boolean }>(
	solution: Solution,
	examples: Example[],
	verifier: Verifier<Solution, Example, Result>,
	stopOnFirstFailure: boolean
): Promise<Candidate<Solution, Example, Result>> {
	const results: Array<{ example: Example; result: Result }> = [];
	let score = 0;
	if (stopOnFirstFailure) {
		for (const ex of examples) {
			const result = await verifier.verify(solution, ex);
			results.push({ example: ex, result });
			if (!result.success) break;
			score++;
		}
	} else {
		const verifications = await Promise.all(
			examples.map(async (ex) => {
				const result = await verifier.verify(solution, ex);
				return { example: ex, result };
			})
		);
		verifications.forEach(({ result }) => {
			if (result.success) score++;
		});
		results.push(...verifications);
	}
	return { solution, results, score };
}

/**
 * repairLoop
 *
 * Attempts to repair the solution based on failing examples until either all
 * processed examples pass or the maximum number of retries is reached.
 */
async function repairLoop<Solution, Example, Result extends { success: boolean }>(
	currentSolution: Solution,
	failingExamples: Example[],
	failingResults: Result[],
	processedExamples: Example[],
	generator: Generator<Solution, Example, Result>,
	verifier: Verifier<Solution, Example, Result>,
	maxRetries: number
): Promise<{ candidate: Candidate<Solution, Example, Result> | null; solution: Solution }> {
	for (let attempt = 1; attempt <= maxRetries; attempt++) {
		const repairedSolution = await generator.repairSolution(currentSolution, failingExamples, failingResults);
		const candidate = await evaluateSolution(repairedSolution, processedExamples, verifier, false);
		if (candidate.score === processedExamples.length) {
			console.log(`[Repair Attempt ${attempt}] ✅ All ${processedExamples.length} examples now passing.`);
			return { candidate, solution: repairedSolution };
		} else {
			console.log(`[Repair Attempt ${attempt}] ❌ Still failing on some examples or introduced regressions.`);
		}
		currentSolution = repairedSolution; // update for next attempt
	}
	return { candidate: null, solution: currentSolution };
}

/**
 * runIncrementalLoop
 *
 * Processes examples one-by-one. After each new example is added,
 * it performs a cumulative evaluation and logs a single consolidated
 * status message.
 */
async function runIncrementalLoop<Solution, Example, Result extends { success: boolean }>(
	generator: Generator<Solution, Example, Result>,
	verifier: Verifier<Solution, Example, Result>,
	examples: Example[],
	options: InferenceOptions
): Promise<Candidate<Solution, Example, Result>> {
	const maxRetries = options.maxRetries ?? 5;
	const repairAll = options.repairAllFailingExamples ?? false;
	const stopOnFirstFailure = options.stopOnFirstFailure ?? false;

	// Initialize with the first example.
	let currentSolution: Solution = await generator.generateInitialSolution([examples[0]]);
	let candidate = await evaluateSolution(currentSolution, [examples[0]], verifier, stopOnFirstFailure);
	let bestCandidate = candidate;
	console.log(`[Snippet #1/${examples.length}] Passed ${candidate.score}/1 so far.`);

	// Process each subsequent example.
	for (let i = 1; i < examples.length; i++) {
		const ex = examples[i];
		// Verify the new snippet.
		const singleResult = await verifier.verify(currentSolution, ex);
		candidate.results.push({ example: ex, result: singleResult });
		if (singleResult.success) candidate.score++;

		// If the new snippet fails, attempt repairs.
		if (!singleResult.success) {
			const failingExamples: Example[] = [];
			const failingResults: Result[] = [];
			if (repairAll) {
				candidate.results.forEach(({ example, result }) => {
					if (!result.success) {
						failingExamples.push(example);
						failingResults.push(result);
					}
				});
			} else {
				failingExamples.push(ex);
				failingResults.push(singleResult);
			}
			const { candidate: repairedCandidate, solution: repairedSolution } = await repairLoop(
				currentSolution,
				failingExamples,
				failingResults,
				examples.slice(0, i + 1),
				generator,
				verifier,
				maxRetries
			);
			if (repairedCandidate) {
				currentSolution = repairedSolution;
				candidate = repairedCandidate;
				if (candidate.score > bestCandidate.score) {
					bestCandidate = candidate;
					if (options.checkpointHook) options.checkpointHook(bestCandidate);
				}
			}
		}
		// Always re-evaluate cumulatively.
		candidate = await evaluateSolution(currentSolution, examples.slice(0, i + 1), verifier, stopOnFirstFailure);
		if (candidate.score > bestCandidate.score) {
			bestCandidate = candidate;
			if (options.checkpointHook) options.checkpointHook(bestCandidate);
		}
		console.log(`[Snippet #${i + 1}/${examples.length}] Passed ${candidate.score}/${i + 1} so far.`);
		if (options.messageCompressor) {
			// Optionally compress messages if needed.
		}
	}
	return bestCandidate;
}

/**
 * runBatchLoop
 *
 * Processes all examples at once. In each iteration, it identifies the failing examples,
 * attempts a repair, and updates the best candidate.
 */
async function runBatchLoop<Solution, Example, Result extends { success: boolean }>(
	generator: Generator<Solution, Example, Result>,
	verifier: Verifier<Solution, Example, Result>,
	examples: Example[],
	options: InferenceOptions
): Promise<Candidate<Solution, Example, Result>> {
	const maxRetries = options.maxRetries ?? 5;
	const stopOnFirstFailure = options.stopOnFirstFailure ?? false;

	let currentSolution: Solution = await generator.generateInitialSolution(examples);
	let candidate = await evaluateSolution(currentSolution, examples, verifier, stopOnFirstFailure);
	let bestCandidate = candidate;
	let retryCount = 0;
	while (retryCount < maxRetries && candidate.score < examples.length) {
		const failingExamples: Example[] = [];
		const failingResults: Result[] = [];
		candidate.results.forEach(({ example, result }) => {
			if (!result.success) {
				failingExamples.push(example);
				failingResults.push(result);
			}
		});
		if (failingExamples.length === 0) break;
		currentSolution = await generator.repairSolution(currentSolution, failingExamples, failingResults);
		candidate = await evaluateSolution(currentSolution, examples, verifier, stopOnFirstFailure);
		if (candidate.score > bestCandidate.score) {
			bestCandidate = candidate;
			if (options.checkpointHook) options.checkpointHook(candidate);
		}
		retryCount++;
		if (options.messageCompressor) {
			// Optionally compress messages.
		}
	}
	return bestCandidate;
}

/**
 * runInferenceLoop
 *
 * Dispatches to the incremental or batch processing loop based on options.
 */
export async function runInferenceLoop<Solution, Example, Result extends { success: boolean }>(
	generator: Generator<Solution, Example, Result>,
	verifier: Verifier<Solution, Example, Result>,
	examples: Example[],
	options?: InferenceOptions
): Promise<Candidate<Solution, Example, Result>> {
	const opts = options ?? {};
	if (opts.incremental) {
		return await runIncrementalLoop(generator, verifier, examples, opts);
	} else {
		return await runBatchLoop(generator, verifier, examples, opts);
	}
}
