type CodeOverlay = {
  line: number;
  text: string;
}

type Code = {
  relFilePath: string;
  code: string;
  overlays: CodeOverlay[];
  editable: boolean;
}

type Repository = {
  files: Code[];
}

class DiffEditor {
  private repository: Repository;
  private currentFile: Code | undefined;

  constructor(repository: Repository) {
    this.repository = repository;
    this.currentFile = undefined;
  }

  getCurrentFile(): Code | undefined {
    return this.currentFile;
  }
}