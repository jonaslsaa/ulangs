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
    failingResults: Result[],
    lastExampleWasNotSolved: boolean
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
  verify(solution: Solution, example: Example): Promise<Result | null>;
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
  incrementalForInitial?: boolean; // Process examples one-by-one
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
      if (result === null) continue;
      results.push({ example: ex, result });
      if (!result.success) break;
      score++;
    }
  } else {
    // Verify all examples in parallel, then accumulate pass/fail
    const verifications = (await Promise.all(
      examples.map(async (ex) => {
        const result = await verifier.verify(solution, ex);
        return { example: ex, result };
      })
    )).filter(v => v.result !== null) as Array<{ example: Example; result: Result }>;
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
async function repairLoop<Solution, Example extends { fileName: string }, Result extends { success: boolean }>(
  currentSolution: Solution,
  failingExamples: Example[],
  failingResults: Result[],
  processedExamples: Example[],
  generator: Generator<Solution, Example, Result>,
  verifier: Verifier<Solution, Example, Result>,
  maxRetries: number,
  stopOnFirstFailure: boolean,
  lastExampleWasNotSolved: boolean,
): Promise<{ candidate: Candidate<Solution, Example, Result> | null; solution: Solution }> {
  let bestSolution: Solution = currentSolution;
  let bestScore = 0;
  
  // First, evaluate the current solution to get its baseline score
  const baselineCandidate = await evaluateSolution(currentSolution, processedExamples, verifier, stopOnFirstFailure);
  bestScore = baselineCandidate.score;
  
  let currentAttemptSolution = currentSolution;
  
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    if (failingExamples.length === 1) {
      console.log(`[Repair Attempt ${attempt}/${maxRetries}] ⏳ Repairing ${failingExamples[0].fileName}...`);
    } else {
      console.log(`[Repair Attempt ${attempt}/${maxRetries}] ⏳ Repairing ${failingExamples.length} examples...`);
    }
    // 1) Ask the generator to repair
    const repairedSolution = await generator.repairSolution(currentAttemptSolution, failingExamples, failingResults, lastExampleWasNotSolved);
    lastExampleWasNotSolved = false; // reset this flag in this scope

    // 2) Evaluate the repaired solution over the processed examples
    const candidate = await evaluateSolution(repairedSolution, processedExamples, verifier, stopOnFirstFailure);
    
    // 3) If improved, store a repair checkpoint candidate and use it as the new baseline
    if (candidate.score > bestScore) {
      bestScore = candidate.score;
      bestSolution = repairedSolution;
      currentAttemptSolution = repairedSolution; // Only update the base for next repair if it's better
      console.log(`[Repair Attempt ${attempt}] ✅ Improved solution with score ${candidate.score}/${processedExamples.length}`);
    } else {
      console.log(`[Repair Attempt ${attempt}] ❌ No improvement or regression (score: ${candidate.score}/${processedExamples.length})`);
      // Don't update currentAttemptSolution, keep using the better solution
    }

    // 4) Check if the repair solved all issues
    if (candidate.score === processedExamples.length) {
      console.log(`[Repair Attempt ${attempt}] ✅ All ${processedExamples.length} examples now passing.`);
      return { candidate, solution: repairedSolution };
    }
  }

  console.log("[Repair Attempts] ❌ All attempts failed. Using best solution so far with score " + bestScore);

  // If we exhaust maxRetries without fully fixing, return the best we got (null candidate means no perfect fix)
  return { candidate: null, solution: bestSolution };
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
async function runLoop<Solution, Example extends { fileName: string }, Result extends { success: boolean }>(
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

  // Maintain a list of skipped examples (those we can't solve and don't want to try to repair)
  const skippedExamples: Example[] = [];

  // 1) Initialize with the first example
  let examplesForInitialGuess = examples;
  if (options.incrementalForInitial) examplesForInitialGuess = [examples[0]]
  let currentSolution: Solution = await generator.generateInitialSolution(examplesForInitialGuess);

  // 2) Evaluate on just that first example
  let candidate = await evaluateSolution(currentSolution, [examples[0]], verifier, stopOnFirstFailure);
  let bestCandidate = candidate;
  checkpoints.push(candidate);

  console.log(`[Snippet #1/${examples.length}] Passed ${candidate.score}/1 so far.`);

  // 3) Process subsequent examples in sequence
  let lastExampleWasNotSolved = false;
  for (let i = 0; i < examples.length; i++) {
    const ex = examples[i];

    // a) Verify the new snippet in isolation
    const singleResult = await verifier.verify(currentSolution, ex);
    if (singleResult === null) continue;
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
        maxRetries,
        stopOnFirstFailure,
        lastExampleWasNotSolved,
      );
      lastExampleWasNotSolved = false; // reset this flag in this scope as well

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
      } else {
        lastExampleWasNotSolved = true; // Set flag for next iteration
        skippedExamples.push(examples[i]); // Add to skipped examples
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
 * runInferenceLoop
 *
 * Dispatches to the incremental or batch processing loop based on options.
 * Returns the best final candidate after either finishing all snippets or
 * reaching the max number of retries.
 */
export async function runInferenceLoop<Solution, Example extends { fileName: string }, Result extends { success: boolean }>(
  generator: Generator<Solution, Example, Result>,
  verifier: Verifier<Solution, Example, Result>,
  examples: Example[],
  options?: InferenceOptions
): Promise<Candidate<Solution, Example, Result>> {
  const opts = options ?? {};
  return await runLoop(generator, verifier, examples, opts);
}