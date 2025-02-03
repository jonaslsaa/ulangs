import path from "path";
import fs from 'fs';

export function _createDirectory(name: string, baseDirectory: string) {
		const baseRelPath = path.join('.tmp', baseDirectory);
		const basePath = path.join(process.cwd(), baseRelPath);
		if (!fs.existsSync(basePath)) {
				fs.mkdirSync(basePath);
		}
		const tempPath = path.join(basePath, name);
		if (!fs.existsSync(tempPath)) {
				fs.mkdirSync(tempPath);
		}
		return tempPath;
}

export function _createWorkingDirectory(baseDirectory: string) {
		const timestamp = new Date().toISOString().replaceAll(':', '-');
		return _createDirectory(timestamp, baseDirectory);
}

export function findAllCodeFiles(directory: string, extension: string, recursive: boolean): string[] {
  if (extension.startsWith('.')) {
    extension = extension.substring(1);
  }

  let codeFiles: string[] = [];

  const entries = fs.readdirSync(directory, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = path.join(directory, entry.name);
    if (entry.isFile() && entry.name.endsWith(`.${extension}`)) {
      codeFiles.push(fullPath);
    } else if (recursive && entry.isDirectory()) {
      codeFiles.push(...findAllCodeFiles(fullPath, extension, recursive));
    }
  }
  return codeFiles;
}

export function _createTemporaryFile(dir: string, fileName: string, temporaryFileDirectoryRecords: Set<string>) {
		const tempPath = path.join(dir, '.tmp');
		if (!fs.existsSync(tempPath)) {
				fs.mkdirSync(tempPath);
		}
		temporaryFileDirectoryRecords.add(tempPath);
		const tempFilePath = path.join(tempPath, fileName);
		if (!fs.existsSync(tempFilePath)) {
				fs.writeFileSync(tempFilePath, '');
		}
		return tempFilePath;
}

export function loadFile(filePath: string | undefined): string | undefined {
		if (filePath === undefined) return undefined;
		if (!fs.existsSync(filePath)) return undefined;
		console.log(`Loading file from ${filePath}`);
		return fs.readFileSync(filePath, 'utf8');
}