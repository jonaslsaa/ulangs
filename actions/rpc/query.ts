import { z } from 'zod';
import type { RPCFunction } from '../rpc';
import { executePrologQuery, generatePrologQuery } from '../query';
import { GetQuery } from '../../rules/queries/mapping';

const argumentSchema = z.object({
	targetPath: z.string(),
	lexerPath: z.string(),
	parserPath: z.string(),
	adapterPath: z.string(),
	queryName: z.string(),
});


const runQuery = {
	argumentSchema,
	method: async (payload: z.infer<typeof argumentSchema>) => {
		const query = GetQuery(payload.queryName);
		const prolog = await generatePrologQuery(
			payload.targetPath,
			payload.lexerPath,
			payload.parserPath,
			payload.adapterPath,
			query
		);
		return executePrologQuery(prolog.filePath);
	},
} satisfies RPCFunction;


export default runQuery;