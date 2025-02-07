export type Result<T> = {
  value?: T;
  error?: string;
  unwrap: () => T;
  unwrapOr: (defaultValue: T) => T;
  unwrapOrElse: (defaultValueFn: () => T) => T;
  isOk: () => boolean;
  isErr: () => boolean;
  map: <U>(fn: (value: T) => U) => Result<U>;
  flatMap: <U>(fn: (value: T) => Result<U>) => Result<U>;
};

export function Ok<T>(value: T): Result<T> {
  return {
    value,
    error: undefined,
    unwrap: () => value,
    unwrapOr: () => value,
    unwrapOrElse: () => value,
    isOk: () => true,
    isErr: () => false,
    map: (fn) => Ok(fn(value)),
    flatMap: (fn) => fn(value)
  };
}

export class UnwrapError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'UnwrapError';
  }
}

export function Err<T>(error: string): Result<T> {
  return {
    value: undefined,
    error,
    unwrap: () => { throw new UnwrapError(error); },
    unwrapOr: (defaultValue) => defaultValue,
    unwrapOrElse: (defaultValueFn) => defaultValueFn(),
    isOk: () => false,
    isErr: () => true,
    map: () => Err(error),
    flatMap: () => Err(error)
  };
}