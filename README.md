# uLangs - Universal Language Server Toolkit

A toolkit that simplifies language server development by automating grammar and semantic analysis generation. Given sample code from any domain-specific language, uLangs generates the necessary parsing and analysis components to create a LSP server implementation. The system uses LLMs to automate the traditionally manual tasks of grammar definition and semantic rule creation.

## Development Status

This project represents ongoing Master's thesis research in automated language server development. Many features are still missing, and the codebase is not yet stable.

## Features

- **Automated LSP Generation**: Creates complete language servers from code samples without manual grammar writing
- **Semantic Analysis**: Extracts language rules and patterns to provide intelligent code assistance
- **Code Intelligence**: Delivers syntax highlighting, completions, diagnostics, and symbol navigation
- **Extensible Pipeline**: Modular design supporting both traditional (ANTLR/Prolog) and LLM-based analysis
- **Language Agnostic**: Works with any textual DSL or programming language given sufficient code examples
- **Universal Client**: Includes ready-to-use VSCode extension that automatically integrates with any generated language server

## Project Structure

- **antlr-grammar-checker/**: Contains grammar definitions and Java/Kotlin code for ANTLR parsing and error handling.
- **lsp-server/**: Implements the server-side logic for handling LSP requests.
- **actions/**: Contains scripts for grammar inference and query generation.
- **rules/**: Prolog rules and queries for semantic analysis.
- **test-dsl/**: Provides test files for various languages to validate grammar and server functionality.

## Getting Started

### Prerequisites

- **Node.js**: Ensure that Node.js is installed on your system.
- **Java**: Required for running ANTLR grammar checks.
- **SWI-Prolog**: Needed for executing Prolog queries.

### Installation

1. **Clone the Repository**:
   ```bash
   git clone <repository-url>
   cd ulangs
   ```

2. **Install Dependencies**:
   ```bash
   pnpm install
   ```

### Running the Language Server

1. **Build the universal-language-server (vscode extension)**:
   ```bash
   pnpm run build:lsp
   ```

2. **Launch the Server**:
   Use the provided VSCode launch configurations to start the language server.

### Command-Line Interface

- **Generate Prolog Query**:
  ```bash
  pnpm run cli query <target> <lexer> <parser> [options]
  ```

- **Check Grammar**:
  ```bash
  pnpm run cli check <directory> <extension> <lexer> <parser>
  ```

- **Infer Grammar**:
  ```bash
  pnpm run cli infer-grammar <directory> <extension> [outputDir] [options]
  ```