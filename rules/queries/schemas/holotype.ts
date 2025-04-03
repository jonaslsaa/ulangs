import { z } from 'zod';
import { DefinitionQueryResultSchema } from './definitions';
import { ASTSchema } from './printAST';

const HolotypeSchema = z.object({
  ast: ASTSchema, // an array of AST nodes
  symbols: DefinitionQueryResultSchema.shape.symbols, // the "symbols" array from definitions
});

export { HolotypeSchema };
export type Holotype = z.infer<typeof HolotypeSchema>;
