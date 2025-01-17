import z from 'zod'
import health from './rpc/health';
import query from './rpc/query';

export type RPCFunction = {
	argumentSchema: z.ZodObject<any> | undefined;
	method: (payload?: any) => Record<string, any>;
};

export const registeredFunctions = {
	health,
	query
} as const;

type NamesOfRegisteredFunctions = keyof typeof registeredFunctions;

export function resolveRPC(functionName: string, payload: string) {
	const registeredFunction = registeredFunctions[functionName as NamesOfRegisteredFunctions] as RPCFunction;
	if (!registeredFunction) {
		console.error(`Function ${functionName} not found`);
		return;
	}

	if (!registeredFunction.argumentSchema) {
		console.log(registeredFunction.method());
	} else {
		const parsedPayload = registeredFunction.argumentSchema.parse(JSON.parse(payload));
		console.log(registeredFunction.method(parsedPayload));
	}
}