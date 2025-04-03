import type { ZodSchema } from 'zod';
import { DefinitionQueryResultSchema } from './schemas/definitions';
import fs from 'fs';
import path from 'path';
import { ASTSchema } from './schemas/printAST';
import { HolotypeSchema } from './schemas/holotype';

type AnyString = string & {};

// The minimal schema type for our base queries
type _Query = {
  path: string; // This path is relative to the rules/queries directory
  schema: ZodSchema;
};

// This is our final shape: it's the original _Query plus a content string.
export type Query = _Query & {
  content: string;
};

// The definition of the known queries (without file content, for now).
const queries = {
  definitions: {
    path: 'definitions.pl',
    schema: DefinitionQueryResultSchema,
  },
  printAST: {
    path: 'printAST.pl',
    schema: ASTSchema,
  },
  holotype: {
    path: 'holotype.pl',
    schema: HolotypeSchema,
  },
} as const;

const queryNames = Object.keys(queries) as (keyof typeof queries)[];

// This gives you the literal type of all query names
type QueryNames = typeof queryNames[number];

function relativePathToLessRelativePath(relPath: string): string {
  return path.join('rules', 'queries', relPath);
}

// Preload all queries at module load time, so fs reads happen only once.
const preloadedQueries: { [K in QueryNames]: Query } = Object.entries(queries).reduce(
  (acc, [name, queryObj]) => {
    acc[name as QueryNames] = {
      ...queryObj,
      content: fs.readFileSync(relativePathToLessRelativePath(queryObj.path), 'utf8'),
    };
    return acc;
  },
  {} as { [K in QueryNames]: Query }
);

/**
 * Get a single query by name (or throw if not found).
 */
export function GetQuery(queryName: (keyof typeof queries) | AnyString): Query {
  const query = preloadedQueries[queryName as QueryNames];
  if (!query) {
    throw new Error(`Query "${queryName}" not found`);
  }
  return query;
}

export function QueryNames(): string[] {
  return queryNames;
}