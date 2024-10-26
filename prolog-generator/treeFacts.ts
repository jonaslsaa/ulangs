import { Parser, ParserRuleContext, TerminalNode } from "antlr4";

interface PrologNode {
  id: number;
  type: string;
  value?: string;
  children: number[];
}

function cleanValue(value: string): string {
  // Clean up newlines and escape quotes
  return value
    .replace(/\n/g, '\\n')
    .replace(/'/g, "\\'")
}

function asserted<T>(value: T): T {
  if (value === undefined || value === null) {
    throw new Error('Value is undefined or null');
  }
  if (typeof value === 'string' && value.trim() === '') {
    throw new Error('Value is empty');
  }
  if (typeof value === 'number' && isNaN(value)) {
    throw new Error('Value is NaN');
  }
  return value;
}

export function generatePrologFacts(tree: ParserRuleContext, parser: Parser): string[] {
  const nodes: Map<ParserRuleContext, PrologNode> = new Map();
  let nodeCounter = 1;

  function createNodes(node: ParserRuleContext): number | null {
    const nodeId = nodeCounter++;
    const nodeType = node.constructor.name;
    const prologNode: PrologNode = {
      id: nodeId,
      type: nodeType,
      children: []
    };

     // Handle terminal nodes differently
    if (node instanceof TerminalNode) {
      const token = node.symbol;
      const tokenTypeName = parser.getSymbolicNames()[token.type];
      const prologNode: PrologNode = {
        id: nodeId,
        type: tokenTypeName || 'UNKNOWN_TOKEN',
        value: cleanValue(node.getText().trim()),
        children: []
      };
      if (prologNode.type === 'UNKNOWN_TOKEN' && prologNode.value?.includes('EOF')) {
        prologNode.type = 'EOF';
      }
      nodes.set(node, prologNode);
      return nodeId;
    }

    // Handle terminal nodes
    if (node.getChildCount() === 0) {
      throw new Error(`Node ${node.constructor.name} has no children`);
    }

    nodes.set(node, prologNode);

    // Process children
    if (node.children) {
      for (const child of node.children) {
        const childId = createNodes(child as ParserRuleContext);
        if (childId !== null) {
          prologNode.children.push(childId);
        }
      }
    }

    return nodeId;
  }

  function generateFacts(): string[] {
    const facts: string[] = [];

    // Add discontiguous declarations
    facts.push(':- discontiguous node/2.');
    facts.push(':- discontiguous has_child/2.');
    facts.push(':- discontiguous has_value/2.');
    facts.push('');

    // Group nodes by type for better readability
    const nodesByType = new Map<string, PrologNode[]>();
    for (const node of nodes.values()) {
      const nodes = nodesByType.get(node.type) || [];
      nodes.push(node);
      nodesByType.set(node.type, nodes);
    }

    // Generate node type declarations grouped by type
    for (const [type, typeNodes] of nodesByType) {
      typeNodes.forEach(node => {
        facts.push(`node(${node.id}, '${asserted(node.type)}').`);
      });
    }
    facts.push('');

    // Generate node values (only for meaningful values)
    const valueDeclarations = Array.from(nodes.values())
      .filter(node => node.value !== undefined && node.value.trim() !== '')
      .map(node => `has_value(${node.id}, '${asserted(node.value)}').`);
    facts.push(...valueDeclarations);
    facts.push('');

    // Generate tree structure with comments
    const childDeclarations = Array.from(nodes.values())
      .flatMap(node =>
        node.children.map(childId => {
          const childNode = Array.from(nodes.values()).find(n => n.id === childId);
          const comment = childNode ? ` % ${node.type} -> ${childNode.type}` : '';
          return `has_child(${node.id}, ${childId}).${comment}`;
        })
      );
    facts.push(...childDeclarations);

    return facts;
  }

  // Create node structure
  createNodes(tree);

  // Generate and return facts
  return generateFacts();
}