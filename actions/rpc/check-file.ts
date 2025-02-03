import { z } from 'zod';
import type { RPCFunction } from '../rpc';
import { checkGrammar } from '../../syntactic/check-grammar';

const argumentSchema = z.object({
	targetPath: z.string(),
	lexerPath: z.string(),
	parserPath: z.string(),
});


const checkFile = {
	argumentSchema,
	method: async (payload: z.infer<typeof argumentSchema>) => {
		return await checkGrammar(payload.lexerPath, payload.parserPath, payload.targetPath);
	},
} satisfies RPCFunction;


export default checkFile;