interface Location {
  col: number;
  file: string;
  len: number;
  line: number;
}

interface Reference extends Location {}

interface Symbol {
  kind: number;
  location: Location;
  name: string;
  references: Reference[];
}

export interface DefinitionQueryResult {
  symbols: Symbol[];
}