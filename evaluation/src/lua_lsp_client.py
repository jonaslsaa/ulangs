import subprocess
import os
import json
import logging
from pathlib import Path
from typing import Optional, Union, List, Dict, Any
import argparse # Added for potential command-line testing

# Import client components
from pylspclient.lsp_endpoint import LspEndpoint
from pylspclient.lsp_client import LspClient
from pylspclient.json_rpc_endpoint import JsonRpcEndpoint

# Configure logging for debugging LSP communication (optional)
# Set level to WARNING or ERROR for less noise when used as a library
# For testing with __main__, INFO can be useful.
logging.basicConfig(level=logging.INFO) # Changed to INFO for __main__ testing
logger = logging.getLogger(__name__)


def get_definition_from_lua_server(
    server_binary_path: str,
    lua_file_path: str,
    line: int, # 0-based line
    character: int, # 0-based character (column)
) -> Optional[Union[Dict[str, Any], List[Dict[str, Any]]]]:
    """
    Connects to the lua-language-server via stdio, opens a Lua file,
    and requests the definition of the symbol at the given position.

    Args:
        server_binary_path: Absolute path to the lua-language-server executable.
        lua_file_path: Absolute path to the Lua file.
        line: 0-based line number for the definition request.
        character: 0-based character number (column) for the definition request.

    Returns:
        The raw dictionary or list of dictionaries returned by the server
        for the definition request (typically a LocationLink or list of LocationLink),
        or None if no definition is found or an error occurs.
        Returns None if the server path or file path is invalid.
    """
    if not os.path.exists(server_binary_path):
        logger.error(f"Server binary not found: {server_binary_path}")
        return None
    if not os.path.exists(lua_file_path):
        logger.error(f"Lua file not found: {lua_file_path}")
        return None

    lsp_client = None
    lsp_endpoint = None # Keep endpoint reference
    proc = None
    try:
        # Use INFO level for process start/end within the function for potential debugging
        logger.info(f"Starting server: {server_binary_path}")
        # Start the language server process using stdio
        proc = subprocess.Popen(
            [server_binary_path, "--stdio"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, # Capture stderr for debugging
            bufsize=0, # Use unbuffered I/O
        )

        # Create JSON-RPC endpoint for stdio
        json_rpc_endpoint = JsonRpcEndpoint(proc.stdin, proc.stdout)
        # Create LSP endpoint
        lsp_endpoint = LspEndpoint(json_rpc_endpoint) # Assign to variable
        # Create LSP client (needed for initialize, initialized, didOpen etc.)
        # The LspClient constructor should start the endpoint.
        lsp_client = LspClient(lsp_endpoint)

        # --- Initialize LSP Connection ---
        root_path = os.path.dirname(lua_file_path)
        root_uri = Path(root_path).as_uri()
        workspace_folders = [{"name": "workspace", "uri": root_uri}]

        # Manually construct capabilities dictionary
        capabilities: Dict[str, Any] = {
            "textDocument": {
                "synchronization": {"didSave": True},
                "completion": {"completionItem": {"snippetSupport": False}},
                "definition": {"linkSupport": True}, # Indicate client supports LocationLink
                "references": {} # Client supports 'textDocument/references'
            },
            "workspace": {
                "applyEdit": True,
                "workspaceFolders": True,
                "configuration": True,
            }
        }

        # Manually construct initialize parameters dictionary
        initialize_params: Dict[str, Any] = {
            "processId": os.getpid(),
            "rootPath": None, # Deprecated but required by pylspclient signature
            "rootUri": root_uri,
            "capabilities": capabilities,
            "initializationOptions": None, # Or server-specific options
            "trace": "off",
            "workspaceFolders": workspace_folders,
        }

        logger.info("Sending Initialize request...")
        # Use lsp_client for initialize as it wraps the call correctly
        init_result = lsp_client.initialize(**initialize_params)
        logger.info(f"Initialize result: {init_result}")

        # Use lsp_client for initialized notification
        lsp_client.initialized()
        logger.info("Sent Initialized notification.")

        # --- Open the Lua Document ---
        file_uri = Path(lua_file_path).as_uri()
        try:
            with open(lua_file_path, "r", encoding="utf-8") as f:
                file_content = f.read()
        except Exception as e:
            logger.error(f"Error reading Lua file {lua_file_path}: {e}")
            return None

        # Manually construct TextDocumentItem dictionary
        text_document_item: Dict[str, Any] = {
            "uri": file_uri,
            "languageId": "lua",
            "version": 1,
            "text": file_content
        }
        # Manually construct DidOpenTextDocumentParams dictionary
        did_open_params: Dict[str, Any] = {
            "textDocument": text_document_item
        }

        logger.info(f"Sending textDocument/didOpen for {file_uri}...")
        # Use lsp_client for didOpen notification
        lsp_client.didOpen(textDocument=did_open_params['textDocument'])
        logger.info("Sent textDocument/didOpen notification.")

        # --- Request Definition ---
        # Manually construct TextDocumentIdentifier dictionary
        text_document_identifier: Dict[str, Any] = {"uri": file_uri}
        # Manually construct Position dictionary
        position: Dict[str, Any] = {"line": line, "character": character}

        # Parameters for the call
        definition_params_dict: Dict[str, Any] = {
                "textDocument": text_document_identifier,
                "position": position
                # workDoneToken and partialResultToken could be added here if needed
        }

        logger.info(
            f"Sending textDocument/definition request for {file_uri} at {line}:{character}..."
        )
        # *** Call lsp_endpoint.call_method directly to get raw result ***
        definition_result_raw = lsp_endpoint.call_method(
            "textDocument/definition",
            **definition_params_dict
        )

        logger.info(f"Received raw definition result: {definition_result_raw}")

        # Return the raw dictionary or list of dictionaries
        return definition_result_raw


    except Exception as e:
        # Log error with traceback for debugging when used as library
        logger.error(f"An error occurred during LSP communication: {e}", exc_info=True)
        # Log stderr from the server process if it exited unexpectedly
        if proc and proc.poll() is not None:
            try:
                stderr_output = proc.stderr.read().decode("utf-8", errors="ignore")
                logger.error(f"Server stderr:\n{stderr_output}")
            except Exception as stderr_e:
                logger.error(f"Failed to read server stderr: {stderr_e}")
        return None
    finally:
        # --- Shutdown ---
        # Use lsp_client for shutdown/exit
        if lsp_client:
            try:
                logger.info("Sending Shutdown request...")
                lsp_client.shutdown()
                logger.info("Sending Exit notification...")
                lsp_client.exit()
                logger.info("LSP connection closed.")
            except Exception as e:
                logger.warning(f"Error during LSP shutdown/exit via client: {e}")

        if proc:
            try:
                # Ensure streams are closed and process terminates
                if proc.stdin and not proc.stdin.closed:
                    proc.stdin.close()
                if proc.stdout and not proc.stdout.closed:
                    proc.stdout.close()
                if proc.stderr and not proc.stderr.closed:
                    proc.stderr.close()

                if proc.poll() is None: # Only terminate if still running
                    proc.terminate() # Try terminating gracefully
                    proc.wait(timeout=5) # Wait a bit
                    logger.info(f"Server process terminated with code: {proc.returncode}")
                else:
                    logger.info(f"Server process already terminated with code: {proc.returncode}")

            except subprocess.TimeoutExpired:
                logger.warning("Server process did not terminate gracefully after 5s, killing.")
                proc.kill()
                proc.wait() # Wait for kill to complete
                logger.info(f"Server process killed.")
            except Exception as e:
                logger.warning(f"Error closing server process: {e}")


def get_references_from_lua_server(
    server_binary_path: str,
    lua_file_path: str,
    line: int, # 0-based line
    character: int, # 0-based character (column)
    include_declaration: bool = True
) -> Optional[List[Dict[str, Any]]]:
    """
    Connects to the lua-language-server via stdio, opens a Lua file,
    and requests all references of the symbol at the given position.

    Args:
        server_binary_path: Absolute path to the lua-language-server executable.
        lua_file_path: Absolute path to the Lua file.
        line: 0-based line number for the references request.
        character: 0-based character number (column) for the references request.
        include_declaration: Whether to include the declaration of the symbol
                             in the list of references.

    Returns:
        A list of Location dictionaries (each with 'uri' and 'range')
        returned by the server for the references request, or None if no
        references are found or an error occurs.
        Returns None if the server path or file path is invalid.
    """
    if not os.path.exists(server_binary_path):
        logger.error(f"Server binary not found: {server_binary_path}")
        return None
    if not os.path.exists(lua_file_path):
        logger.error(f"Lua file not found: {lua_file_path}")
        return None

    lsp_client = None
    lsp_endpoint = None
    proc = None
    try:
        logger.info(f"Starting server for references: {server_binary_path}")
        proc = subprocess.Popen(
            [server_binary_path, "--stdio"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=0,
        )

        json_rpc_endpoint = JsonRpcEndpoint(proc.stdin, proc.stdout)
        lsp_endpoint = LspEndpoint(json_rpc_endpoint)
        lsp_client = LspClient(lsp_endpoint)

        # --- Initialize LSP Connection ---
        root_path = os.path.dirname(lua_file_path)
        root_uri = Path(root_path).as_uri()
        workspace_folders = [{"name": "workspace", "uri": root_uri}]

        capabilities: Dict[str, Any] = {
            "textDocument": {
                "synchronization": {"didSave": True},
                "completion": {"completionItem": {"snippetSupport": False}},
                "definition": {"linkSupport": True},
                "references": {} # Client indicates support for references
            },
            "workspace": {
                "applyEdit": True,
                "workspaceFolders": True,
                "configuration": True,
            }
        }

        initialize_params: Dict[str, Any] = {
            "processId": os.getpid(),
            "rootPath": None,
            "rootUri": root_uri,
            "capabilities": capabilities,
            "initializationOptions": None,
            "trace": "off",
            "workspaceFolders": workspace_folders,
        }

        logger.info("Sending Initialize request (references)...")
        init_result = lsp_client.initialize(**initialize_params)
        logger.info(f"Initialize result (references): {init_result}")

        lsp_client.initialized()
        logger.info("Sent Initialized notification (references).")

        # --- Open the Lua Document ---
        file_uri = Path(lua_file_path).as_uri()
        try:
            with open(lua_file_path, "r", encoding="utf-8") as f:
                file_content = f.read()
        except Exception as e:
            logger.error(f"Error reading Lua file {lua_file_path}: {e}")
            return None

        text_document_item: Dict[str, Any] = {
            "uri": file_uri, "languageId": "lua", "version": 1, "text": file_content
        }
        did_open_params: Dict[str, Any] = {"textDocument": text_document_item}

        logger.info(f"Sending textDocument/didOpen for {file_uri} (references)...")
        lsp_client.didOpen(textDocument=did_open_params['textDocument'])
        logger.info("Sent textDocument/didOpen notification (references).")

        # --- Request References ---
        text_document_identifier: Dict[str, Any] = {"uri": file_uri}
        position: Dict[str, Any] = {"line": line, "character": character}
        context: Dict[str, Any] = {"includeDeclaration": include_declaration}

        references_params_dict: Dict[str, Any] = {
            "textDocument": text_document_identifier,
            "position": position,
            "context": context
        }

        logger.info(
            f"Sending textDocument/references request for {file_uri} at {line}:{character} (includeDeclaration: {include_declaration})..."
        )
        references_result_raw = lsp_endpoint.call_method(
            "textDocument/references",
            **references_params_dict
        )

        logger.info(f"Received raw references result: {references_result_raw}")

        # The result should be a list of Location objects or null.
        # Ensure it's a list or None before returning.
        if references_result_raw is None:
            return None
        if isinstance(references_result_raw, list):
            return references_result_raw
        else:
            logger.warning(f"Expected list or null for references, got {type(references_result_raw)}")
            return None # Or handle as an error appropriately

    except Exception as e:
        logger.error(f"An error occurred during LSP communication (references): {e}", exc_info=True)
        if proc and proc.poll() is not None:
            try:
                stderr_output = proc.stderr.read().decode("utf-8", errors="ignore")
                logger.error(f"Server stderr (references):\n{stderr_output}")
            except Exception as stderr_e:
                logger.error(f"Failed to read server stderr (references): {stderr_e}")
        return None
    finally:
        # --- Shutdown ---
        if lsp_client:
            try:
                logger.info("Sending Shutdown request (references)...")
                lsp_client.shutdown()
                logger.info("Sending Exit notification (references)...")
                lsp_client.exit()
                logger.info("LSP connection closed (references).")
            except Exception as e:
                logger.warning(f"Error during LSP shutdown/exit via client (references): {e}")

        if proc:
            try:
                if proc.stdin and not proc.stdin.closed: proc.stdin.close()
                if proc.stdout and not proc.stdout.closed: proc.stdout.close()
                if proc.stderr and not proc.stderr.closed: proc.stderr.close()
                if proc.poll() is None:
                    proc.terminate()
                    proc.wait(timeout=5)
                    logger.info(f"Server process terminated with code: {proc.returncode} (references)")
                else:
                    logger.info(f"Server process already terminated with code: {proc.returncode} (references)")
            except subprocess.TimeoutExpired:
                logger.warning("Server process did not terminate gracefully after 5s, killing (references).")
                proc.kill()
                proc.wait()
                logger.info("Server process killed (references).")
            except Exception as e:
                logger.warning(f"Error closing server process (references): {e}")

if __name__ == "__main__":
    # --- Configuration (matches manual_compare.py for consistency) ---
    LUA_SERVER_BINARY_PATH = "/Users/jonassilva/Downloads/lua-language-server-3.14.0-darwin-arm64/bin/lua-language-server"
    EVALUATION_LUA_FILE_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"

    # --- Test Target Position (0-based for LSP) ---
    # Example: "GameObject" in "function GameObject:new(area, x, y, opts)"
    # In evaluation-sample.lua, this is line 3, col 10 (1-based)
    # For LSP (0-based): line 2, character 9
    DEFAULT_TEST_LINE = 2
    DEFAULT_TEST_CHAR = 9

    parser = argparse.ArgumentParser(description="Test LSP client for finding references.")
    parser.add_argument("--line", type=int, default=DEFAULT_TEST_LINE, help="Line number (0-based) to query.")
    parser.add_argument("--char", type=int, default=DEFAULT_TEST_CHAR, help="Character number (0-based) to query.")
    parser.add_argument("--server-path", type=str, default=LUA_SERVER_BINARY_PATH, help="Path to lua-language-server binary.")
    parser.add_argument("--file-path", type=str, default=EVALUATION_LUA_FILE_PATH, help="Path to the Lua file to analyze.")
    parser.add_argument("--no-declaration", action="store_false", dest="include_declaration", help="Do not include declaration in references.")
    args = parser.parse_args()

    print("--- Testing get_references_from_lua_server ---")
    print(f"Lua Server: {args.server_path}")
    print(f"Lua File: {args.file_path}")
    print(f"Position: Line {args.line}, Character {args.char} (0-based)")
    print(f"Include Declaration: {args.include_declaration}")
    print("-" * 40)

    # --- Input Validation ---
    paths_valid = True
    if not os.path.exists(args.server_path):
        logger.error(f"Lua server binary not found: {args.server_path}")
        paths_valid = False
    if not os.path.exists(args.file_path):
        logger.error(f"Evaluation Lua file not found: {args.file_path}")
        paths_valid = False

    if not paths_valid:
        print("\nPlease correct the file paths before running.")
    else:
        references = get_references_from_lua_server(
            server_binary_path=args.server_path,
            lua_file_path=args.file_path,
            line=args.line,
            character=args.char,
            include_declaration=args.include_declaration
        )

        print("\n--- Lua LSP Server References Result ---")
        if references is not None:
            print(json.dumps(references, indent=2, sort_keys=True))
        else:
            print("No references found or an error occurred.")
        print("-" * 40)
