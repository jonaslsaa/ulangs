import { z } from 'zod';
import { type RPCFunction } from '../actions/rpc';

/**
 * If an RPCFunction has an argumentSchema, we want to accept typed arguments.
 * If argumentSchema is undefined, then we accept no arguments.
 */
type PrepareRPC<T extends RPCFunction> =
  T['argumentSchema'] extends z.ZodTypeAny
    ? (args: z.infer<T['argumentSchema']>) => ReturnType<T['method']>
    : () => ReturnType<T['method']>;

/**
 * prepareRPC returns a callable that:
 *   - Parses the arguments with `argumentSchema` (if defined).
 *   - Invokes the RPCFunction's `method` with the parsed args (or `undefined` if no schema).
 */
export function prepareRPC<T extends RPCFunction>(fn: T): PrepareRPC<T> {
  if (fn.argumentSchema) {
    // If there's a schema, parse input and pass it to fn.method
    return ((args: unknown) => {
      const parsedArgs = fn.argumentSchema!.parse(args);
      return fn.method(parsedArgs);
    }) as PrepareRPC<T>;
  } else {
    // If there's no schema, just call fn.method with no arguments
    return (() => {
      return fn.method(undefined);
    }) as PrepareRPC<T>;
  }
}