import json
import os
import logging
from pathlib import Path
from typing import Optional, Union, List, Dict, Any
import argparse

# Configure logging
logging.basicConfig(level=logging.WARNING) # Default level
logger = logging.getLogger(__name__)

def _json_loc_to_lsp_location_dict(
    loc_data: Dict[str, Any],
    file_uri: str
) -> Optional[Dict[str, Any]]:
    """
    Converts a JSON location entry (1-based line, 0-based col, len)
    to an LSP Location dictionary (0-based line/char range).
    """
    line_1based = loc_data.get("line")
    col_0based = loc_data.get("col")
    length = loc_data.get("len")

    if line_1based is None or col_0based is None or length is None:
        logger.warning(f"Skipping incomplete location data: {loc_data}")
        return None

    # Convert to 0-based LSP standard
    lsp_start_line = line_1based - 1
    lsp_start_char = col_0based
    lsp_end_line = line_1based - 1  # Assuming locations are single-line in the JSON
    lsp_end_char = col_0based + length

    return {
        "uri": file_uri,
        "range": {
            "start": {"line": lsp_start_line, "character": lsp_start_char},
            "end": {"line": lsp_end_line, "character": lsp_end_char}
        }
    }

def get_definition_from_json(
    json_data_path: str,
    source_file_path: str,
    line: int, # 0-based line (LSP standard)
    character: int, # 0-based character (column) (LSP standard)
) -> Optional[Union[Dict[str, Any], List[Dict[str, Any]]]]:
    """
    Finds the definition location for a symbol at a given position
    by looking it up in a pre-defined JSON data structure and returns
    it in a format mimicking LSP LocationLink. Handles JSON data where
    'line' is 1-based and 'col' is 0-based.

    Args:
        json_data_path: Absolute path to the JSON file containing symbol data.
        source_file_path: Absolute path to the source file being analyzed
                          (used to resolve "__file__" and create URIs).
        line: 0-based line number of the symbol usage/reference (LSP standard).
        character: 0-based character number (column) of the symbol usage/reference (LSP standard).

    Returns:
        A list containing a single dictionary representing the definition
        location in LSP LocationLink format (as a raw dictionary), or None
        if no corresponding symbol or definition is found.
        Returns None if file paths are invalid or JSON is malformed.
    """
    if not os.path.exists(json_data_path):
        logger.error(f"JSON data file not found: {json_data_path}")
        return None
    if not os.path.exists(source_file_path):
        logger.error(f"Source file not found: {source_file_path}")
        return None

    try:
        with open(json_data_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except json.JSONDecodeError as e:
        logger.error(f"Error decoding JSON file {json_data_path}: {e}")
        return None
    except Exception as e:
        logger.error(f"Error reading JSON file {json_data_path}: {e}")
        return None

    if "symbols" not in data or not isinstance(data["symbols"], list):
        logger.error(f"Invalid JSON structure: 'symbols' list not found in {json_data_path}")
        return None

    source_file_uri = Path(source_file_path).as_uri()

    found_symbol_definition_loc = None
    origin_selection_range_data = None # Store the range of the clicked symbol

    for symbol_entry in data["symbols"]:
        # Check if the symbol structure is valid
        if not isinstance(symbol_entry, dict) or "location" not in symbol_entry:
            logger.warning(f"Skipping invalid symbol entry (missing location): {symbol_entry.get('name', 'N/A')}")
            continue

        # Check if the cursor position falls within the definition location itself
        loc = symbol_entry.get("location")
        if isinstance(loc, dict):
            loc_line_1based = loc.get("line") # JSON line is 1-based
            loc_col_0based = loc.get("col")   # JSON col is 0-based
            loc_len = loc.get("len")

            if (loc_line_1based is not None and loc_col_0based is not None and loc_len is not None and
                    (loc_line_1based - 1) == line and # Adjust JSON line to 0-based for comparison
                    character >= loc_col_0based and character < (loc_col_0based + loc_len)):
                found_symbol_definition_loc = loc # Store the actual definition loc
                origin_selection_range_data = loc # Clicked on the definition itself
                break # Found the symbol

        # Check if the cursor position falls within any reference location
        refs = symbol_entry.get("references", [])
        if not isinstance(refs, list):
             logger.warning(f"Invalid references format for symbol {symbol_entry.get('name', 'N/A')}: {refs}")
             # Continue to next symbol, as this one might be malformed but others could be fine
             continue

        for ref in refs:
            if not isinstance(ref, dict):
                logger.warning(f"Skipping invalid reference entry: {ref} for symbol {symbol_entry.get('name', 'N/A')}")
                continue

            ref_line_1based = ref.get("line") # JSON line is 1-based
            ref_col_0based = ref.get("col")   # JSON col is 0-based
            ref_len = ref.get("len")

            if (ref_line_1based is not None and ref_col_0based is not None and ref_len is not None and
                    (ref_line_1based - 1) == line and
                    character >= ref_col_0based and character < (ref_col_0based + ref_len)):
                # Found the symbol via one of its references
                found_symbol_definition_loc = symbol_entry.get("location") # Definition of the symbol whose reference was clicked
                origin_selection_range_data = ref # The reference that was clicked
                break # Found the symbol

        if found_symbol_definition_loc:
            break # Exit outer loop once symbol is found

    if (found_symbol_definition_loc and isinstance(found_symbol_definition_loc, dict) and
        origin_selection_range_data and isinstance(origin_selection_range_data, dict)):

        origin_line_1based = origin_selection_range_data.get("line")
        origin_col_0based = origin_selection_range_data.get("col")
        origin_len = origin_selection_range_data.get("len")

        def_line_1based = found_symbol_definition_loc.get("line")
        def_col_0based = found_symbol_definition_loc.get("col")
        def_len = found_symbol_definition_loc.get("len")

        if (def_line_1based is not None and def_col_0based is not None and def_len is not None and
            origin_line_1based is not None and origin_col_0based is not None and origin_len is not None):

            lsp_origin_start_line = origin_line_1based - 1
            lsp_origin_start_char = origin_col_0based
            lsp_origin_end_line = origin_line_1based - 1
            lsp_origin_end_char = origin_col_0based + origin_len

            lsp_target_start_line = def_line_1based - 1
            lsp_target_start_char = def_col_0based
            lsp_target_end_line = def_line_1based - 1
            lsp_target_end_char = def_col_0based + def_len

            location_link_dict = {
                "targetUri": source_file_uri,
                "targetRange": {
                    "start": {"line": lsp_target_start_line, "character": lsp_target_start_char},
                    "end": {"line": lsp_target_end_line, "character": lsp_target_end_char}
                },
                "targetSelectionRange": {
                    "start": {"line": lsp_target_start_line, "character": lsp_target_start_char},
                    "end": {"line": lsp_target_end_line, "character": lsp_target_end_char}
                },
                "originSelectionRange": {
                    "start": {"line": lsp_origin_start_line, "character": lsp_origin_start_char},
                    "end": {"line": lsp_origin_end_line, "character": lsp_origin_end_char}
                }
            }
            return [location_link_dict]
        else:
            logger.warning(f"Found symbol but its location or origin data is incomplete: Def={found_symbol_definition_loc}, Origin={origin_selection_range_data}")
            return None
    else:
        logger.info(f"No symbol definition found for L{line}:C{character} (0-based) in {json_data_path} via {source_file_path}")
        return None


def get_references_from_json(
    json_data_path: str,
    source_file_path: str,
    line: int, # 0-based line (LSP standard)
    character: int, # 0-based character (column) (LSP standard)
    include_declaration: bool = True
) -> Optional[List[Dict[str, Any]]]:
    """
    Finds all references for a symbol at a given position by looking it up
    in a pre-defined JSON data structure. Returns them as a list of LSP
    Location objects. Handles JSON data where 'line' is 1-based and 'col' is 0-based.

    Args:
        json_data_path: Absolute path to the JSON file containing symbol data.
        source_file_path: Absolute path to the source file being analyzed.
        line: 0-based line number of the symbol usage/reference.
        character: 0-based character number (column) of the symbol usage/reference.
        include_declaration: Whether to include the symbol's declaration in the results.

    Returns:
        A list of LSP Location dictionaries, or None if the symbol is not found
        or an error occurs. Returns an empty list if the symbol is found but has
        no references and include_declaration is False.
    """
    if not os.path.exists(json_data_path):
        logger.error(f"JSON data file not found: {json_data_path}")
        return None
    if not os.path.exists(source_file_path):
        logger.error(f"Source file not found: {source_file_path}")
        return None

    try:
        with open(json_data_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except json.JSONDecodeError as e:
        logger.error(f"Error decoding JSON file {json_data_path}: {e}")
        return None
    except Exception as e:
        logger.error(f"Error reading JSON file {json_data_path}: {e}")
        return None

    if "symbols" not in data or not isinstance(data["symbols"], list):
        logger.error(f"Invalid JSON structure: 'symbols' list not found in {json_data_path}")
        return None

    source_file_uri = Path(source_file_path).as_uri()
    target_symbol_entry = None

    for symbol_entry in data["symbols"]:
        if not isinstance(symbol_entry, dict) or "location" not in symbol_entry:
            logger.warning(f"Skipping invalid symbol entry (missing location): {symbol_entry.get('name', 'N/A')}")
            continue

        # Check if cursor is on the definition
        loc = symbol_entry.get("location")
        if isinstance(loc, dict):
            loc_line_1based = loc.get("line")
            loc_col_0based = loc.get("col")
            loc_len = loc.get("len")
            if (loc_line_1based is not None and loc_col_0based is not None and loc_len is not None and
                    (loc_line_1based - 1) == line and
                    character >= loc_col_0based and character < (loc_col_0based + loc_len)):
                target_symbol_entry = symbol_entry
                break

        # Check if cursor is on any reference
        refs = symbol_entry.get("references", [])
        if not isinstance(refs, list):
            logger.warning(f"Invalid references format for symbol {symbol_entry.get('name', 'N/A')}: {refs}")
            continue

        for ref in refs:
            if not isinstance(ref, dict):
                logger.warning(f"Skipping invalid reference entry: {ref} for symbol {symbol_entry.get('name', 'N/A')}")
                continue
            ref_line_1based = ref.get("line")
            ref_col_0based = ref.get("col")
            ref_len = ref.get("len")
            if (ref_line_1based is not None and ref_col_0based is not None and ref_len is not None and
                    (ref_line_1based - 1) == line and
                    character >= ref_col_0based and character < (ref_col_0based + ref_len)):
                target_symbol_entry = symbol_entry
                break
        if target_symbol_entry:
            break

    if not target_symbol_entry:
        logger.info(f"No symbol found for references at L{line}:C{character} (0-based) in {json_data_path} via {source_file_path}")
        return None

    # Symbol found, collect its references and optionally its declaration
    all_locations: List[Dict[str, Any]] = []

    # Add declaration if requested
    if include_declaration:
        declaration_loc_data = target_symbol_entry.get("location")
        if isinstance(declaration_loc_data, dict):
            lsp_loc = _json_loc_to_lsp_location_dict(declaration_loc_data, source_file_uri)
            if lsp_loc:
                all_locations.append(lsp_loc)
        else:
            logger.warning(f"Symbol {target_symbol_entry.get('name', 'N/A')} has invalid location data.")


    # Add all references
    references_data = target_symbol_entry.get("references", [])
    if isinstance(references_data, list):
        for ref_data in references_data:
            if isinstance(ref_data, dict):
                lsp_loc = _json_loc_to_lsp_location_dict(ref_data, source_file_uri)
                if lsp_loc:
                    all_locations.append(lsp_loc)
            else:
                logger.warning(f"Skipping invalid reference item data: {ref_data} for symbol {target_symbol_entry.get('name', 'N/A')}")
    else:
        logger.warning(f"Symbol {target_symbol_entry.get('name', 'N/A')} has invalid references data format.")

    return all_locations


if __name__ == "__main__":
    # --- Configuration for testing ---
    # These paths should ideally match your local setup or be configurable.
    # Using paths similar to manual_compare.py and lua_lsp_client.py for consistency.
    DEFAULT_LUA_SERVER_BINARY_PATH = "/Users/jonassilva/Downloads/lua-language-server-3.14.0-darwin-arm64/bin/lua-language-server"
    DEFAULT_EVALUATION_LUA_FILE_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"
    DEFAULT_SYMBOL_DATA_JSON_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-definitions.json" # Path to your JSON

    # --- Test Target Position (0-based for LSP) ---
    # Example: "GameObject" in "function GameObject:new(area, x, y, opts)"
    # In evaluation-sample.lua, this is line 3, col 10 (1-based)
    # For LSP (0-based): line 2, character 9
    DEFAULT_TEST_LINE = 2
    DEFAULT_TEST_CHAR = 9

    parser = argparse.ArgumentParser(description="Test Mock LSP client for definitions and references.")
    parser.add_argument(
        "--mode", type=str, choices=["definition", "references"], default="references",
        help="Operation mode: 'definition' or 'references'."
    )
    parser.add_argument("--line", type=int, default=DEFAULT_TEST_LINE, help="Line number (0-based) to query.")
    parser.add_argument("--char", type=int, default=DEFAULT_TEST_CHAR, help="Character number (0-based) to query.")
    parser.add_argument("--json-path", type=str, default=DEFAULT_SYMBOL_DATA_JSON_PATH, help="Path to the symbol JSON data file.")
    parser.add_argument("--file-path", type=str, default=DEFAULT_EVALUATION_LUA_FILE_PATH, help="Path to the source Lua file.")
    parser.add_argument("--no-declaration", action="store_false", dest="include_declaration", help="Do not include declaration in references (for references mode).")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging for the script.")

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.INFO) # Set root logger to INFO for this script
        logger.setLevel(logging.INFO)


    print(f"--- Testing Mock LSP Client ({args.mode}) ---")
    print(f"Symbol JSON: {args.json_path}")
    print(f"Source File: {args.file_path}")
    print(f"Position: Line {args.line}, Character {args.char} (0-based)")
    if args.mode == "references":
        print(f"Include Declaration: {args.include_declaration}")
    print("-" * 40)

    paths_valid = True
    if not os.path.exists(args.json_path):
        logger.error(f"Symbol JSON data file not found: {args.json_path}")
        paths_valid = False
    if not os.path.exists(args.file_path):
        logger.error(f"Source Lua file not found: {args.file_path}")
        paths_valid = False

    if not paths_valid:
        print("\nPlease correct the file paths before running.")
    else:
        if args.mode == "definition":
            definition_result = get_definition_from_json(
                json_data_path=args.json_path,
                source_file_path=args.file_path,
                line=args.line,
                character=args.char
            )
            print("\n--- Mock JSON Definition Result ---")
            if definition_result is not None:
                print(json.dumps(definition_result, indent=2, sort_keys=True))
            else:
                print("No definition found or an error occurred.")
        elif args.mode == "references":
            references_result = get_references_from_json(
                json_data_path=args.json_path,
                source_file_path=args.file_path,
                line=args.line,
                character=args.char,
                include_declaration=args.include_declaration
            )
            print("\n--- Mock JSON References Result ---")
            if references_result is not None: # Could be an empty list, which is valid
                print(json.dumps(references_result, indent=2, sort_keys=True))
            else: # Only None means symbol not found or major error
                print("No symbol found for references or an error occurred.")
        print("-" * 40)
