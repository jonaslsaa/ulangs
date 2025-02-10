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

import type { OpenAIMessage } from './utils';

/** Generator interface: implement to generate or repair a solution. */
export interface Generator<Solution, Example, Result> {
  /**
   * Generates an initial solution from a set of examples.
   */
  generateInitialSolution(examples: Example[]): Promise<Solution>;

  /**
   * Repairs a failing solution, given the failing examples and results.
   */
  repairSolution(
    oldSolution: Solution,
    failingExamples: Example[],
    failingResults: Result[]
  ): Promise<Solution>;

	/**
	 * Stores messages generated by the LLM.
	 */
  messages: OpenAIMessage[];
}

/** Verifier interface: implement to test a solution on a given example. */
export interface Verifier<Solution, Example, Result> {
  /**
   * Verifies a solution on a single example.
   */
  verify(solution: Solution, example: Example): Promise<Result>;
}

/** Candidate type: stores a solution, its test results, and a score. */
export interface Candidate<Solution, Example, Result> {
  solution: Solution;
  results: Array<{ example: Example; result: Result }>;
  score: number; // e.g., number of examples passing
}

/** Options to configure the inference loop. */
export interface InferenceOptions {
  maxRetries?: number;
  stopOnFirstFailure?: boolean;
  incremental?: boolean; // Process examples one-by-one
  repairAllFailingExamples?: boolean; // Pass all failing examples to repairSolution

  /**
   * Optional function for LLM context compression.
   * If used, it might look like: (messages) => someCompressedMessages
   */
  messageCompressor?: (messages: OpenAIMessage[]) => OpenAIMessage[];

  /**
   * Optional hook that is called whenever we find a new best candidate,
   * e.g., for checkpointing to disk or logging externally.
   */
  checkpointHook?: (candidate: Candidate<any, any, any>) => void;
}

/**
 * evaluateSolution
 *
 * Evaluates the given solution on a set of examples using the provided verifier.
 * If `stopOnFirstFailure` is true, it returns early upon encountering the first failure.
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
    // Sequentially test each example until a failure occurs
    for (const ex of examples) {
      const result = await verifier.verify(solution, ex);
      results.push({ example: ex, result });
      if (!result.success) break;
      score++;
    }
  } else {
    // Verify all examples in parallel, then accumulate pass/fail
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
    // 1) Ask the generator to repair
    const repairedSolution = await generator.repairSolution(currentSolution, failingExamples, failingResults);

    // 2) Evaluate the repaired solution over the processed examples
    const candidate = await evaluateSolution(repairedSolution, processedExamples, verifier, false);

    // 3) Check if the repair solved all issues
    if (candidate.score === processedExamples.length) {
      console.log(`[Repair Attempt ${attempt}] ✅ All ${processedExamples.length} examples now passing.`);
      return { candidate, solution: repairedSolution };
    } else {
      console.log(`[Repair Attempt ${attempt}] ❌ Still failing on some examples or introduced regressions.`);
    }

    // 4) If not perfect, keep the new solution as the baseline for the next attempt
    currentSolution = repairedSolution;
  }

  // If we exhaust maxRetries without fully fixing, return the best we got (null candidate means no perfect fix)
  return { candidate: null, solution: currentSolution };
}

/**
 * maybeCompressMessages
 *
 * A helper that checks if a messageCompressor was provided in the options.
 * If so, it applies compression to the generator's messages (assuming the
 * generator stores them).
 */
function maybeCompressMessages<Solution, Example, Result extends { success: boolean }>(
  generator: Generator<Solution, Example, Result>,
  options: InferenceOptions
): void {
  if (options.messageCompressor) {
    // If your generator has a 'messages' array, you could do something like:
    if (Array.isArray(generator.messages)) {
      // Compress in place or reassign
      generator.messages = options.messageCompressor(generator.messages);
    }
  }
}

/**
 * runIncrementalLoop
 *
 * Processes examples one-by-one. After each new example is added,
 * it performs a cumulative evaluation and logs a single consolidated
 * status message. Also stores internal checkpoints of best solutions.
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

  // Maintain a local list of checkpoint candidates
  const checkpoints: Candidate<Solution, Example, Result>[] = [];

  // 1) Initialize with the first example
  let currentSolution: Solution = await generator.generateInitialSolution([examples[0]]);

  // 2) Evaluate on just that first example
  let candidate = await evaluateSolution(currentSolution, [examples[0]], verifier, stopOnFirstFailure);
  let bestCandidate = candidate;
  checkpoints.push(candidate);

  console.log(`[Snippet #1/${examples.length}] Passed ${candidate.score}/1 so far.`);

  // 3) Process subsequent examples in sequence
  for (let i = 1; i < examples.length; i++) {
    const ex = examples[i];

    // a) Verify the new snippet in isolation
    const singleResult = await verifier.verify(currentSolution, ex);
    // b) Merge that into the candidate’s results
    candidate.results.push({ example: ex, result: singleResult });
    if (singleResult.success) {
      candidate.score++;
    }

    // c) If it fails, attempt repairs
    if (!singleResult.success) {
      const failingExamples: Example[] = [];
      const failingResults: Result[] = [];

      if (repairAll) {
        // Add all failing examples so far
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
        // If we got a fully passing solution for all processed examples
        currentSolution = repairedSolution;
        candidate = repairedCandidate;
        // Update bestCandidate if improved
        if (candidate.score > bestCandidate.score) {
          bestCandidate = candidate;
          checkpoints.push(bestCandidate);
          if (options.checkpointHook) options.checkpointHook(bestCandidate);
        }
      }
    }

    // d) Always re-evaluate cumulatively
    candidate = await evaluateSolution(currentSolution, examples.slice(0, i + 1), verifier, stopOnFirstFailure);
    if (candidate.score > bestCandidate.score) {
      bestCandidate = candidate;
      checkpoints.push(bestCandidate);
      if (options.checkpointHook) options.checkpointHook(bestCandidate);
    }

    // e) Log the consolidated result
    console.log(`[Snippet #${i + 1}/${examples.length}] Passed ${candidate.score}/${i + 1} so far.`);

    // f) Optionally compress messages if an LLM is used
    maybeCompressMessages(generator, options);
  }

  return bestCandidate;
}

/**
 * runBatchLoop
 *
 * Processes all examples at once. In each iteration, it identifies the failing examples,
 * attempts a repair, and updates the best candidate if it improves. Also stores
 * internal checkpoints for each new best candidate.
 */
async function runBatchLoop<Solution, Example, Result extends { success: boolean }>(
  generator: Generator<Solution, Example, Result>,
  verifier: Verifier<Solution, Example, Result>,
  examples: Example[],
  options: InferenceOptions
): Promise<Candidate<Solution, Example, Result>> {
  const maxRetries = options.maxRetries ?? 5;
  const stopOnFirstFailure = options.stopOnFirstFailure ?? false;

  // Maintain a local list of checkpoints
  const checkpoints: Candidate<Solution, Example, Result>[] = [];

  // 1) Generate initial solution & evaluate
  let currentSolution: Solution = await generator.generateInitialSolution(examples);
  let candidate = await evaluateSolution(currentSolution, examples, verifier, stopOnFirstFailure);
  let bestCandidate = candidate;
  checkpoints.push(candidate);

  let retryCount = 0;

  // 2) Keep repairing until all pass or we reach maxRetries
  while (retryCount < maxRetries && candidate.score < examples.length) {
    const failingExamples: Example[] = [];
    const failingResults: Result[] = [];

    // Collect all failing examples
    candidate.results.forEach(({ example, result }) => {
      if (!result.success) {
        failingExamples.push(example);
        failingResults.push(result);
      }
    });

    if (failingExamples.length === 0) {
      // Means we pass all, so break out
      break;
    }

    // a) Attempt to repair
    currentSolution = await generator.repairSolution(currentSolution, failingExamples, failingResults);

    // b) Re-evaluate
    candidate = await evaluateSolution(currentSolution, examples, verifier, stopOnFirstFailure);

    // c) If improved, store checkpoint
    if (candidate.score > bestCandidate.score) {
      bestCandidate = candidate;
      checkpoints.push(candidate);
      if (options.checkpointHook) {
        options.checkpointHook(candidate);
      }
    }

    // d) Possibly compress LLM messages
    maybeCompressMessages(generator, options);

    retryCount++;
  }

  return bestCandidate;
}

/**
 * runInferenceLoop
 *
 * Dispatches to the incremental or batch processing loop based on options.
 * Returns the best final candidate after either finishing all snippets or
 * reaching the max number of retries.
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