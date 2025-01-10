import path from "path";
import fs from 'fs';

export function _createTemporaryDirectory(name: string, rootDirectory: string) {
		const baseRelPath = path.join(rootDirectory);
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

export function findAllCodeFiles(directory: string, extension: string, recursive: boolean): string[] {
		if (extension.startsWith('.')) {
				extension = extension.substring(1);
		}

		const files = fs.readdirSync(directory); // Get all files in the directory
		const codeFiles = files.filter(file => file.endsWith(`.${extension}`)); // Filter for code files

		if (recursive) { // If recursive, search subdirectories
				const subdirectories = files.filter(file => fs.statSync(path.join(directory, file)).isDirectory()); // Get subdirectories
				subdirectories.forEach(subdir => { // Iterate over subdirectories and search for code files
						codeFiles.push(...findAllCodeFiles(path.join(directory, subdir), extension, recursive));
				});
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