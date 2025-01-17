import z from 'zod'
import health from './rpc/health';

export type RPCFunction<A extends z.ZodObject<any, any, any> = z.ZodObject<any, any, any>> = {
    name: string;
		argumentSchema: A;
		method: (payload: z.infer<A>) => any;
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