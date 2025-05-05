import os
import json
import logging
from pathlib import Path

# Import the functions from the other modules
from lua_lsp_client import get_definition_from_lua_server
from mock_lsp_client import get_definition_from_json

# Configure logging for this script (optional)
logging.basicConfig(level=logging.INFO) # Show INFO messages from this script
logger = logging.getLogger(__name__)

# --- Configuration ---
LUA_SERVER_BINARY_PATH = "/Users/jonassilva/Downloads/lua-language-server-3.14.0-darwin-arm64/bin/lua-language-server"
EVALUATION_LUA_FILE_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"
SYMBOL_DATA_JSON_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-definitions.json"

import argparse

args = argparse.ArgumentParser(description="Manual comparison of Lua LSP server and mock JSON data.")
args.add_argument("line", type=int, help="Line number (0-based) to evaluate.")
args.add_argument("char", type=int, help="Character number (0-based) to evaluate.")
args = args.parse_args()

TARGET_LINE = args.line
TARGET_CHAR = args.char
# --- End Configuration ---

def main():
    print("--- Manual Comparison ---")
    print(f"Lua Server: {LUA_SERVER_BINARY_PATH}")
    print(f"Lua File: {EVALUATION_LUA_FILE_PATH}")
    print(f"Symbol JSON: {SYMBOL_DATA_JSON_PATH}")
    print(f"Position: Line {TARGET_LINE}, Character {TARGET_CHAR} (0-based)")
    print("-" * 25)

    # --- Input Validation ---
    paths_valid = True
    if not os.path.exists(LUA_SERVER_BINARY_PATH):
        logger.error(f"Lua server binary not found: {LUA_SERVER_BINARY_PATH}")
        paths_valid = False
    if not os.path.exists(EVALUATION_LUA_FILE_PATH):
        logger.error(f"Evaluation Lua file not found: {EVALUATION_LUA_FILE_PATH}")
        paths_valid = False
    if not os.path.exists(SYMBOL_DATA_JSON_PATH):
        logger.error(f"Symbol JSON data file not found: {SYMBOL_DATA_JSON_PATH}")
        paths_valid = False

    if not paths_valid:
        print("\nPlease correct the file paths in the script before running.")
        return
    # --- End Input Validation ---

    # --- Get Definition from Real LSP Server ---
    print("\n[1] Querying Real Lua LSP Server...")
    real_lsp_definition = get_definition_from_lua_server(
        server_binary_path=LUA_SERVER_BINARY_PATH,
        lua_file_path=EVALUATION_LUA_FILE_PATH,
        line=TARGET_LINE,
        character=TARGET_CHAR
    )

    print("\n--- Real LSP Server Result ---")
    if real_lsp_definition is not None:
        # Pretty print the JSON-like dictionary/list with sorted keys
        print(json.dumps(real_lsp_definition, indent=2, sort_keys=True))
    else:
        print("No definition found or error occurred.")
    print("-" * 28)

    # --- Get Definition from Mock JSON Lookup ---
    print("\n[2] Querying Mock JSON Data...")
    mock_definition = get_definition_from_json(
        json_data_path=SYMBOL_DATA_JSON_PATH,
        source_file_path=EVALUATION_LUA_FILE_PATH, # Pass the Lua file path here too
        line=TARGET_LINE,
        character=TARGET_CHAR
    )

    print("\n--- Mock JSON Lookup Result ---")
    if mock_definition is not None:
        # Pretty print the JSON-like dictionary/list with sorted keys
        print(json.dumps(mock_definition, indent=2, sort_keys=True))
    else:
        print("No definition found or error occurred.")
    print("-" * 28)

    # --- Basic Comparison ---
    print("\n--- Comparison ---")
    # Simple equality check (might be too strict if order differs in lists or keys were unsorted)
    # Sorting keys for printing helps visual comparison, but direct == might still fail if values differ.
    if real_lsp_definition == mock_definition:
        print("Results appear to be IDENTICAL.")
    else:
        print("Results appear to be DIFFERENT.")
        # Add more sophisticated comparison logic here if needed,
        # e.g., comparing specific fields like URI and range start/end.
    print("-" * 18)


if __name__ == "__main__":
    main()
