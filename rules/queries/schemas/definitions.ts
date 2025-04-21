import { z } from 'zod';

const LocationSchema = z.object({
  col: z.number(),
  file: z.string(),
  len: z.number(),
  line: z.number(),
});

const ReferenceSchema = LocationSchema;

const SymbolSchema = z.object({
  kind: z.number(),
  location: LocationSchema,
  name: z.string(),
  references: z.array(ReferenceSchema),
});

export const DefinitionQueryResultSchema = z.object({
  symbols: z.array(SymbolSchema),
});

export type Definition = z.infer<typeof SymbolSchema>;
export type DefinitionQueryResult = z.infer<typeof DefinitionQueryResultSchema>;