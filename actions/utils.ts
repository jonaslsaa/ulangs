import type OpenAI from 'openai';

export const Stats = {
		totalRequests: 0,
		totalCompletedRequests: 0,
		inputTokens: 0,
		cachedInputTokens: 0,
		outputTokens: 0,
		get totalTokens () {
				return this.inputTokens + this.outputTokens;
		},
		get avgTokensPerRequest () {
				return this.totalTokens / this.totalRequests;
		},
		getCost(inputPricePerMillionTokens: number, outputPricePerMillionTokens: number) {
				return this.totalTokens * inputPricePerMillionTokens + this.totalTokens * outputPricePerMillionTokens;
		},
		addRequest() {
				this.totalRequests++;
		},
		addCompletedRequest(completion: OpenAI.Chat.Completions.ChatCompletion | undefined) {
			// completion.usage.prompt_tokens, completion.usage.completion_tokens, completion.usage?.prompt_tokens_details?.cached_tokens ?? 0
				if (!completion) return;
				this.totalCompletedRequests++;
				if (!completion.usage) return;
				
				this.inputTokens += completion.usage.prompt_tokens ?? 0;
				this.cachedInputTokens += completion.usage?.prompt_tokens_details?.cached_tokens ?? 0;
				this.outputTokens += completion.usage.completion_tokens ?? 0;
		}
}

export function ExitAndLogStats(exitCode: number = 0) {
		console.log("\n[Stats]");
		console.log(`Generated ${Stats.totalRequests} requests, and completed ${Stats.totalCompletedRequests} requests.`);
		if (Stats.cachedInputTokens === 0) {
				console.log(`    Input tokens: ${Stats.inputTokens}, Output tokens: ${Stats.outputTokens}`);
		} else {
				const NonCachedTokens = Stats.inputTokens - Stats.cachedInputTokens;
				console.log(`    Cached input tokens: ${Stats.cachedInputTokens}, Non-cached input tokens: ${NonCachedTokens}, Output tokens: ${Stats.outputTokens}`);
		}

		console.log(`    ${Stats.totalTokens} tokens (${Stats.avgTokensPerRequest} avg tokens per request)`);
		process.exit(exitCode);
}