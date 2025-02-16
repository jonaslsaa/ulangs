import { z } from 'zod';

// 1) Location schema (similar to LSP's `Location`)
const LocationSchema = z.object({
  file: z.string(),
  line: z.number(),
  col: z.number(),
  len: z.number(),
});

// 2) Enum-like schema for `nodeKind`
const NodeKindSchema = z.union([
  z.literal("declaration"),
  z.literal("reference"),
  z.null(),
]);

// 3) Define TypeScript type for AST node
type ASTNode = {
  nodeName: string;
  nodeKind: "declaration" | "reference" | null;
  symbolKind?: string; // e.g., "function", "variable", "class"...
  name?: string;
  location?: z.infer<typeof LocationSchema>;
  children: ASTNode[];
};

// 4) Define Zod schema using a manual type hint
const ASTNodeSchema: z.ZodType<ASTNode> = z.object({
  nodeName: z.string(),
  nodeKind: NodeKindSchema,
  symbolKind: z.string().optional(),
  name: z.string().optional(),
  location: LocationSchema.optional(),
  children: z.lazy(() => ASTNodeSchema.array()), // Recursive reference
});

// 5) The full AST is an array of root-level nodes
const ASTSchema = z.array(ASTNodeSchema);

export { ASTNodeSchema, ASTSchema, LocationSchema };
export type { ASTNode };