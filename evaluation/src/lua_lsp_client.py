import subprocess
import os
import json
import logging
from pathlib import Path
from typing import Optional, Union, List, Dict, Any

# Import client components
from pylspclient.lsp_endpoint import LspEndpoint
from pylspclient.lsp_client import LspClient
from pylspclient.json_rpc_endpoint import JsonRpcEndpoint

# Configure logging for debugging LSP communication (optional)
# Set level to WARNING or ERROR for less noise when used as a library
logging.basicConfig(level=logging.WARNING)
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
        for the definition request, or None if no definition is found or
        an error occurs.
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