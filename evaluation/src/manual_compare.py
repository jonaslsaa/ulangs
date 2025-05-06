import os
import json
import logging
from pathlib import Path
import argparse

# Import the functions from the other modules
from lua_lsp_client import get_definition_from_lua_server, get_references_from_lua_server
from mock_lsp_client import get_definition_from_json, get_references_from_json

# Configure logging for this script (optional)
logging.basicConfig(level=logging.INFO) # Show INFO messages from this script
logger = logging.getLogger(__name__)

# --- Configuration ---
# These are defaults and can be overridden by command-line arguments if desired,
# but for simplicity, we'll keep them as constants here as per the original script.
LUA_SERVER_BINARY_PATH = "/Users/jonassilva/Downloads/lua-language-server-3.14.0-darwin-arm64/bin/lua-language-server"
EVALUATION_LUA_FILE_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"
SYMBOL_DATA_JSON_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-definitions.json"
# --- End Configuration ---

def _make_hashable(obj):
    """
    Recursively converts a potentially nested structure of dictionaries and lists
    into a hashable representation (tuples of sorted items for dicts, tuples for lists).
    """
    if isinstance(obj, dict):
        return tuple(sorted((k, _make_hashable(v)) for k, v in obj.items()))
    if isinstance(obj, list):
        return tuple(_make_hashable(elem) for elem in obj)
    # Assume other types (int, str, bool, None) are already hashable
    return obj

def _normalize_result_for_set_comparison(result_data):
    """
    Normalizes LSP result data (None, a single dict, or a list of dicts)
    into a set of hashable representations for order-independent comparison.
    """
    if result_data is None:
        return set()
    
    # Ensure result_data is treated as a list of items for uniform processing
    items_to_process = []
    if isinstance(result_data, list):
        items_to_process = result_data
    elif isinstance(result_data, dict):
        items_to_process = [result_data] # Treat a single dict as a list with one item
    else:
        # This case should ideally not happen for valid LSP definition/references results
        logger.warning(f"Unexpected data type for normalization: {type(result_data)}. Value: {result_data}")
        # Attempt to make it hashable directly, or return a unique marker if it's unhashable
        try:
            return {_make_hashable(result_data)}
        except TypeError:
            return {str(result_data)} # Fallback to string representation if truly unhashable

    # Process each item (dictionary) in the list
    hashable_items = set()
    for item in items_to_process:
        if isinstance(item, dict):
            hashable_items.add(_make_hashable(item))
        else:
            # If an item in a list is not a dict, this is unusual for LSP Location/LocationLink
            logger.warning(f"Unexpected item type in list for normalization: {type(item)}. Value: {item}")
            try:
                hashable_items.add(_make_hashable(item))
            except TypeError:
                 hashable_items.add(str(item)) # Fallback
    return hashable_items

def main():
    parser = argparse.ArgumentParser(description="Manual comparison of Lua LSP server and mock JSON data.")
    parser.add_argument("line", type=int, help="Line number (0-based) to evaluate.")
    parser.add_argument("char", type=int, help="Character number (0-based) to evaluate.")
    parser.add_argument(
        "-M", "--mode", type=str, choices=["DEF", "REF"], required=True,
        help="Operation mode: 'DEF' for definitions, 'REF' for references."
    )
    parser.add_argument(
        "--include-declaration", action=argparse.BooleanOptionalAction, default=True,
        help="For 'REF' mode, whether to include the declaration in the references list (default: --include-declaration)."
    )
    parser.add_argument("--server-path", type=str, default=LUA_SERVER_BINARY_PATH, help="Path to lua-language-server binary.")
    parser.add_argument("--lua-file-path", type=str, default=EVALUATION_LUA_FILE_PATH, help="Path to the Lua file to analyze.")
    parser.add_argument("--json-path", type=str, default=SYMBOL_DATA_JSON_PATH, help="Path to the symbol JSON data file.")

    args = parser.parse_args()

    TARGET_LINE = args.line
    TARGET_CHAR = args.char
    MODE = args.mode.upper()
    INCLUDE_DECLARATION = args.include_declaration

    current_lua_server_path = args.server_path
    current_lua_file_path = args.lua_file_path
    current_json_path = args.json_path

    print("--- Manual Comparison ---")
    print(f"Mode: {MODE}")
    print(f"Lua Server: {current_lua_server_path}")
    print(f"Lua File: {current_lua_file_path}")
    print(f"Symbol JSON: {current_json_path}")
    print(f"Position: Line {TARGET_LINE}, Character {TARGET_CHAR} (0-based)")
    if MODE == "REF":
        print(f"Include Declaration in References: {INCLUDE_DECLARATION}")
    print("-" * 25)

    paths_valid = True
    if not os.path.exists(current_lua_server_path):
        logger.error(f"Lua server binary not found: {current_lua_server_path}")
        paths_valid = False
    if not os.path.exists(current_lua_file_path):
        logger.error(f"Evaluation Lua file not found: {current_lua_file_path}")
        paths_valid = False
    if not os.path.exists(current_json_path):
        logger.error(f"Symbol JSON data file not found: {current_json_path}")
        paths_valid = False

    if not paths_valid:
        print("\nPlease correct the file paths (either in script constants or via arguments) before running.")
        return

    real_lsp_result = None
    mock_result = None

    if MODE == "DEF":
        print("\n[1] Querying Real Lua LSP Server for Definition...")
        real_lsp_result = get_definition_from_lua_server(
            server_binary_path=current_lua_server_path,
            lua_file_path=current_lua_file_path,
            line=TARGET_LINE,
            character=TARGET_CHAR
        )
        print("\n[2] Querying Mock JSON Data for Definition...")
        mock_result = get_definition_from_json(
            json_data_path=current_json_path,
            source_file_path=current_lua_file_path,
            line=TARGET_LINE,
            character=TARGET_CHAR
        )
    elif MODE == "REF":
        print("\n[1] Querying Real Lua LSP Server for References...")
        real_lsp_result = get_references_from_lua_server(
            server_binary_path=current_lua_server_path,
            lua_file_path=current_lua_file_path,
            line=TARGET_LINE,
            character=TARGET_CHAR,
            include_declaration=INCLUDE_DECLARATION
        )
        print("\n[2] Querying Mock JSON Data for References...")
        mock_result = get_references_from_json(
            json_data_path=current_json_path,
            source_file_path=current_lua_file_path,
            line=TARGET_LINE,
            character=TARGET_CHAR,
            include_declaration=INCLUDE_DECLARATION
        )
    else:
        logger.error(f"Invalid mode: {MODE}. Should be 'DEF' or 'REF'.")
        return

    print(f"\n--- Real LSP Server Result ({MODE}) ---")
    if real_lsp_result is not None:
        print(json.dumps(real_lsp_result, indent=2, sort_keys=True))
    else:
        print(f"No {MODE.lower()} found or error occurred.")
    print("-" * 28)

    print(f"\n--- Mock JSON Lookup Result ({MODE}) ---")
    if mock_result is not None:
        print(json.dumps(mock_result, indent=2, sort_keys=True))
    else:
        print(f"No {MODE.lower()} found or error occurred.")
    print("-" * 28)

    print("\n--- Comparison ---")
    
    tp, fp, fn = 0, 0, 0
    total_golden, total_mock = 0, 0
    comparison_outcome_message = "Results are DIFFERENT" # Default

    if real_lsp_result == mock_result:
        comparison_outcome_message = "Results are IDENTICAL by direct comparison."
        if MODE == "REF":
            # For REF mode, if identical, all items in real are TPs, no FPs or FNs
            real_set_norm = _normalize_result_for_set_comparison(real_lsp_result)
            # mock_set_norm will be the same as real_set_norm
            
            tp = len(real_set_norm)
            fp = 0
            fn = 0
            total_golden = tp # All golden items were found
            total_mock = tp   # All mock items were correct
    else:
        # If direct comparison fails, use set-based comparison
        try:
            real_set_norm = _normalize_result_for_set_comparison(real_lsp_result)
            mock_set_norm = _normalize_result_for_set_comparison(mock_result)

            if real_set_norm == mock_set_norm:
                comparison_outcome_message = "Results are IDENTICAL by set comparison."
                # This case implies structure might differ slightly but content is same
                # e.g. single dict vs list of one dict
            else:
                comparison_outcome_message = "Results are DIFFERENT" # Already default, but explicit
                
                diff_real_only_set = real_set_norm - mock_set_norm
                if diff_real_only_set:
                    print(f"  Items only in Real LSP ({len(diff_real_only_set)}):")
                    for item in diff_real_only_set:
                        print(f"    - {str(item)[:200]}{'...' if len(str(item)) > 200 else ''}")

                diff_mock_only_set = mock_set_norm - real_set_norm
                if diff_mock_only_set:
                    print(f"  Items only in Mock JSON ({len(diff_mock_only_set)}):")
                    for item in diff_mock_only_set:
                        print(f"    - {str(item)[:200]}{'...' if len(str(item)) > 200 else ''}")
                
                if not diff_real_only_set and not diff_mock_only_set and real_lsp_result != mock_result:
                     # This can happen if _normalize_result_for_set_comparison had to use fallback string conversion
                     # for unhashable, non-dict/list items, and those string versions matched.
                    print("  NOTE: Set comparison found no content differences, but direct comparison failed. This might be due to subtle type differences or ordering if normalization wasn't perfect for all data types.")


            # Calculate REF statistics based on set comparison
            if MODE == "REF":
                tp = len(real_set_norm.intersection(mock_set_norm))
                fp = len(mock_set_norm - real_set_norm)
                fn = len(real_set_norm - mock_set_norm)
                total_golden = len(real_set_norm)
                total_mock = len(mock_set_norm)

        except Exception as e:
            comparison_outcome_message = f"  Could not perform detailed set comparison due to an error: {e}"
            logger.error("Error during set comparison", exc_info=True)
            # For REF mode, if comparison fails, we can't reliably calculate TP/FP/FN
            if MODE == "REF":
                # Indicate stats are unavailable
                tp, fp, fn = -1, -1, -1 # Marker for unavailable stats
                total_golden, total_mock = -1, -1


    print(comparison_outcome_message)

    # --- Print REF Statistics (if applicable and calculable) ---
    if MODE == "REF":
        print("\n--- REF Statistics ---")
        if tp != -1: # Check if stats were calculable
            # Standardized TP line with two spaces after "True"
            # The descriptive text "(Correctly matched items)" is general enough.
            print(f"True  Positives (TP): {tp}  (Correctly matched items)")
            print(f"False Positives (FP): {fp}  (Mock returned, but not in Real)")
            print(f"False Negatives (FN): {fn}  (In Real, but Mock missed)")
            print() # Consistent blank line
            print(f"Total in Golden Standard (Real LSP): {total_golden}")
            print(f"Total Returned by Mock Client: {total_mock}")
            
            print("\nCalculated Statistics:")
            precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
            recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0 # same as tp / total_golden if total_golden > 0

            if (precision + recall) > 0:
                f1_score = 2 * (precision * recall) / (precision + recall)
            else:
                f1_score = 0.0
            
            print(f"  Precision: {precision:.2%}")
            print(f"  Recall: {recall:.2%}")
            print(f"  F1 Score: {f1_score:.2%}")
        else:
            print("  Statistics could not be calculated due to a comparison error.")
        print("-" * 22) # Consistent separator

    print("-" * 18) # Final separator for the whole test case

if __name__ == "__main__":
    main()
