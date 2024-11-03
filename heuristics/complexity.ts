const uniqueTokenRegex = /(?:"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')|(?:\/\/.*?$|\/\*[\s\S]*?\*\/)|(?:\b0[xX][0-9a-fA-F]+\b|\b\d*\.\d+(?:[eE][+-]?\d+)?\b|\b\d+\b)|(?:[a-zA-Z_]\w*)|(?:[+\-*\/=<>!&|^~%]+|[\[\]{}(),.;:])|(?:\s+)/gm;

export function calculateComplexity(input: string, method: 'characters' | 'words' | 'lines' | 'gzip' | 'unique' = 'gzip'): number {
    const lines = input.split('\n');
    if (method === 'characters') {
        return lines.join('').length;
    }
    if (method === 'words') {
        return lines.join(' ').split(' ').length;
    }
    if (method === 'lines') {
        return lines.length;
    }
    if (method === 'gzip') {
        const data = Buffer.from(input, 'utf8');
        const compressed = Bun.gzipSync(data);
        return compressed.length;
    }
    if (method === 'unique') {
        const tokens = input.match(uniqueTokenRegex);
        if (!tokens) {
            console.error('Failed to calculate unique tokens, got null');
            return 0;
        }
        return tokens.length;
    }
    throw new Error(`Unknown complexity method: ${method}`);
}