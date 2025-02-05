import { get_encoding } from 'tiktoken';
import type { OpenAIMessage } from './utils';

export function compressCodeBlock(message: string): string {
		const placeholder = "... omitted code block for brevity ...";

		// This regex looks for:
		// 1) Opening triple backticks ```
		// 2) Optional language specifier (captured as group #1)
		// 3) Any text (captured as group #2) until
		// 4) The closing triple backticks ```
		//
		// Explanation:
		// - `[^\n\r]*` means capture up to a newline (language can be letters, numbers, underscores, etc.)
		// - `([\s\S]*?)` is a "lazy" match for everything (including newlines) until the first occurrence of ```
		// - The 'g' flag applies globally so it will replace all code blocks.
		const codeBlockRegex = /```([^\n\r]*)\n([\s\S]*?)```/g;

		return message.replace(codeBlockRegex, (match, maybeLang, content) => {
				// maybeLang might be an empty string if no language was specified
				// Build the replacement:
				// If there's a language, we keep it on the same line as ```
				// Then put our placeholder on a new line, and finally close with ```
				const language = maybeLang.trim();
				return language
						? `\`\`\`${language}\n${placeholder}\n\`\`\``
						: `\`\`\`\n${placeholder}\n\`\`\``;
		});
}

export function compressMessages(messages: OpenAIMessage[]): OpenAIMessage[] {
		return messages.map((message, index) => {
				if (index === 0 || index === messages.length - 1 || index === messages.length - 2) {
						// Don't compress the first or last two messages
						return message;
				}
				// Only compress messages with role 'user'
				if (message.role !== 'user') return message;
				if (message.content) message.content = compressCodeBlock(message.content.toString());
				return message;
		});
}

const encoding = get_encoding("o200k_base") // We just always use the gpt-4o encoding scheme
export function countTokens(messages: OpenAIMessage[]) {
	const TOKEN_CONSTANT_PER_MESSAGE = 2;
	const tokensCount = messages.reduce((acc, message) => {
			const c = message.content?.toString() ?? '';	
			const tokens = encoding.encode(c).length;
			return acc + tokens + TOKEN_CONSTANT_PER_MESSAGE;
	}, 0);
	return tokensCount;
}