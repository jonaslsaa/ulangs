import { z } from 'zod';
import type { RPCFunction } from '../rpc';

const argumentSchema = z.object({});

export default {
    name: 'health',
		argumentSchema,
		method: () => ({
			status: 'ok',
		}),
} satisfies RPCFunction;