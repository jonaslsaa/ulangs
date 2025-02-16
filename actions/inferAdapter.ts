import path from "path";
import fs from "fs";
import { loadSnippetsByComplexity } from "./utils/snippets";
import { loadOpenAIEnvVars, type OpenAIMessage } from "../llm/utils";
import { compressMessages } from "../llm/compress-messages";
import { runInferenceLoop, type InferenceOptions } from "../llm/autoCreationLoop";
import { AdapterGenerator, AdapterVerifier, AdapterContext } from "../llm/adapterPipeline";
import { GetQuery } from "../rules/queries/mapping";
import type { CLIInferAdapterArguments } from "../cli";
import { ExitAndLogStats } from './utils';

/**
 * doInferAdapter
 * 
 * Infers (or repairs) a Prolog adapter file that maps CST -> AST / symbol definitions,
 * ensuring we can run the holotype query on the given code snippets successfully.
 */
export async function doInferAdapter(
  directory: string,
  extension: string,
  outputDir: string,
  options: CLIInferAdapterArguments
): Promise<void> {
  // 1. Load code snippets from the specified directory.
  const snippets = await loadSnippetsByComplexity(directory, extension, options.recursive);
  if (!snippets || snippets.length === 0) {
    console.error("No files found with the given extension.");
    process.exit(1);
    return;
  }
  console.log(`Loaded ${snippets.length} snippet(s) from: ${directory}`);

  // 2. Load the OpenAI environment (API key, model, etc.).
  const openaiEnv = loadOpenAIEnvVars();
  // We'll store conversation history in messages, if needed.
  const messages: OpenAIMessage[] = [];

  // 3. Construct the adapter context.
  //    (Contains references to the parser/lexer and LLM client.)
  const adapterContext = new AdapterContext(options.lexer, options.parser, openaiEnv);

  const holotypeQuery = GetQuery('definitions'); // TODO: investigate what the best holotype query is

  // 4. Create the AdapterGenerator and AdapterVerifier.
  const generator = new AdapterGenerator(
    openaiEnv,
    messages,
    options.lexer,
    options.parser,
    options.initialAdapter,
    holotypeQuery
  );
  const verifier = new AdapterVerifier(adapterContext, snippets, holotypeQuery);

  // 6. Configure inference options to either test each snippet in turn
  //    or do single-pass; here we just do standard incremental usage with
  //    “stopOnFirstFailure: false,” etc.
  const inferenceOptions: InferenceOptions = {
    maxRetries: 5,
    stopOnFirstFailure: true,
    incrementalForInitial: false,
    repairAllFailingExamples: false,
    messageCompressor: compressMessages,
    checkpointHook: (candidate) => {
      // Whenever we get a new best candidate, log the updated score:
      console.log(`Checkpoint: ${candidate.score}/${snippets.length} queries passing so far.`);
    },
  };

  // 7. Run the inference loop, producing an “Adapter” solution that
  //    passes all examples (or as many as possible).
  const candidate = await runInferenceLoop(generator, verifier, snippets, inferenceOptions);

  if (candidate.score < snippets.length) {
    console.warn(`Final solution passes ${candidate.score} out of ${snippets.length} examples (some issues remain).`);
  } else {
    console.log("Final solution passed all examples!");
  }

  // 8. Write the final adapter to the output directory as `MyAdapter.pl`.
  const adapterPath = path.join(outputDir, "MyAdapter.pl");
  fs.writeFileSync(adapterPath, candidate.solution.source, "utf8");
  console.log(`Wrote final adapter to: ${adapterPath}`);

  ExitAndLogStats();
}

