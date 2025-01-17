import z from 'zod'
import health from './rpc/health';

export type RPCFunction = {
    name: string;
		argumentSchema: z.ZodObject<any>;
		method: (payload: any) => any;
};

const registeredFunctions: RPCFunction[] = [
    health,
];

export function resolveRPC(functionName: string, payload: string) {
	const registeredFunction = registeredFunctions.find(func => func.name === functionName);
	if (!registeredFunction) {
		console.error(`Function ${functionName} not found`);
		return;
	}

	const parsedPayload = registeredFunction.argumentSchema.parse(JSON.parse(payload));

	console.log(registeredFunction.method(parsedPayload));
}