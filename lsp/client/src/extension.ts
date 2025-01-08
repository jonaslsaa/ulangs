/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, commands, window, StatusBarAlignment } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;


export function activate(context: ExtensionContext) {
	// Log as client that we are starting
	// The server is implemented in node
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'plaintext' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Add status bar
	context.subscriptions.push(
		commands.registerCommand('uLangs.showStatus', () => {
			window.showInformationMessage('Universal Language Server is running');
		})
	);
	const myStatusBar = window.createStatusBarItem(StatusBarAlignment.Right, 100);
	myStatusBar.command = 'uLangs.showStatus';
	myStatusBar.text = 'uLangs';
	context.subscriptions.push(myStatusBar);
	myStatusBar.show();

	// Get extension configuration
	// TODO

	// Create the language client and start the client.
	client = new LanguageClient(
		'uLangsLSP',
		'uLangs',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start().then(() => {
		client.info("Language client started.");
	});
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
