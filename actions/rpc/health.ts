import { z } from 'zod';
import type { RPCFunction } from '../rpc';

const argumentSchema = z.object({});

export default {
		argumentSchema,
		method: () => ({
			status: 'ok',
		}),
} satisfies RPCFunction;