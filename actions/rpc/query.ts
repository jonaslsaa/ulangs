import { z } from 'zod';
import type { RPCFunction } from '../rpc';
import { executePrologQuery, generatePrologQuery } from '../query';

const argumentSchema = z.object({
	targetPath: z.string(),
	lexerPath: z.string(),
	parserPath: z.string(),
	adapterPath: z.string(),
	queryPath: z.string(),
});


const runQuery = {
	argumentSchema,
	method: async (payload: z.infer<typeof argumentSchema>) => {
		const prolog = await generatePrologQuery(
			payload.targetPath,
			payload.lexerPath,
			payload.parserPath,
			payload.adapterPath,
			payload.queryPath,
		);
		return executePrologQuery(prolog.filePath);
	},
} satisfies RPCFunction;


export default runQuery;