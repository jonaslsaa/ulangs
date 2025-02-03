import z from 'zod'
import health from './rpc/health';
import query from './rpc/query';
import check from './rpc/check-file';

export type RPCFunction = {
	argumentSchema: z.ZodObject<any> | undefined;
	method: (payload?: any) => Promise<Record<string, any>>;
};

export const registeredFunctions = {
	health,
	query,
	check
} as const;

type NamesOfRegisteredFunctions = keyof typeof registeredFunctions;

export async function resolveRPC(functionName: string, payload: string) {
	const registeredFunction = registeredFunctions[functionName as NamesOfRegisteredFunctions] as RPCFunction;
	if (!registeredFunction) {
		console.error(`Function ${functionName} not found`);
		return;
	}

	if (!registeredFunction.argumentSchema) {
		console.log(await registeredFunction.method());
	} else {
		const parsedPayload = registeredFunction.argumentSchema.parse(JSON.parse(payload));
		console.log(await registeredFunction.method(parsedPayload));
	}
}