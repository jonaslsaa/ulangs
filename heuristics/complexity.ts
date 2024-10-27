export function calculateComplexity(input: string, method: 'characters' | 'words' | 'lines' | 'gzip' = 'gzip'): number {
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
    throw new Error(`Unknown complexity method: ${method}`);
}