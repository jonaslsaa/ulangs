import { z } from 'zod';
import { registeredFunctions, type RPCFunction } from '../actions/rpc';
import { spawn } from 'child_process';

/**
 * If an RPCFunction has an argumentSchema, we want to accept typed arguments.
 * If argumentSchema is undefined, then we accept no arguments.
 */
type PrepareRPC<T extends RPCFunction> =
	T['argumentSchema'] extends z.ZodTypeAny
	? (args: z.infer<T['argumentSchema']>) => ReturnType<T['method']>
	: () => ReturnType<T['method']>;


function _callViaCLI(methodName: string, payload: string): Promise<string> {
	return new Promise((resolve, reject) => {
		const childProcess = spawn('pnpm', ['dlx', 'tsx', 'cli.ts', 'rpc', methodName, payload]);

		let stdout = '';
		let stderr = '';

		childProcess.stdout.on('data', (data) => {
			stdout += data.toString();
		});

		childProcess.stderr.on('data', (data) => {
			stderr += data.toString();
		});

		childProcess.on('close', (code) => {
			if (code === 0) {
				// Process completed successfully
				resolve(stdout);
			} else {
				// Process ended with an error code
				reject(new Error(stderr));
			}
		});

		// If there's an error spawning
		childProcess.on('error', (error) => {
			reject(error);
		});
	});
}


async function callAndParse(methodName: string, payload: string): Promise<Record<string, any>> {
	const raw = await _callViaCLI(methodName, payload);
	console.log(raw);
	if (raw.trim() === "") {
		throw new Error("callAndParse: no output")
	}
	const result = JSON.parse(raw);
	if (result.error) {
		throw new Error(result.error);
	}
	return result;
}


/**
 * prepareRPC returns a callable that:
 *   - Parses the arguments with `argumentSchema` (if defined).
 *   - Invokes the RPCFunction's `method` with the parsed args (or `undefined` if no schema).
 */
export function prepareRPC<K extends keyof typeof registeredFunctions>(fnName: K): PrepareRPC<typeof registeredFunctions[K]> {
	const fn = registeredFunctions[fnName];
	if (fn.argumentSchema) {
		// If there's a schema, parse input and pass it to fn.method
		return ((args: unknown) => {
			const parsedArgs = fn.argumentSchema!.parse(args);
			return callAndParse(fnName, JSON.stringify(parsedArgs));
		}) as PrepareRPC<typeof registeredFunctions[K]>;
	} else {
		// If there's no schema, just call fn.method with no arguments
		return (() => {
			return callAndParse(fnName, '{}');
		}) as PrepareRPC<typeof registeredFunctions[K]>;
	}
}