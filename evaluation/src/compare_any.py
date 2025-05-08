import os
import json
import logging
from pathlib import Path
import argparse
from enum import Enum
from typing import Optional

# Import the functions from the other modules
from lua_lsp_client import get_definition_from_lua_server, get_references_from_lua_server
from mock_lsp_client import get_definition_from_json, get_references_from_json
from naive_lsp_client import get_definition_from_naive_client, get_references_from_naive_client

# Configure logging for this script
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# --- Configuration Constants (Defaults) ---
LUA_SERVER_BINARY_PATH = "/Users/jonassilva/Downloads/lua-language-server-3.14.0-darwin-arm64/bin/lua-language-server"
EVALUATION_LUA_FILE_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"
SYMBOL_DATA_JSON_PATH = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-definitions.json"

class ClientType(Enum):
    REAL = "REAL"
    MOCK = "MOCK"
    NAIVE = "NAIVE"

    def __str__(self):
        return self.value

def _make_hashable(obj):
    """
    Recursively converts a potentially nested structure of dictionaries and lists
    into a hashable representation (tuples of sorted items for dicts, tuples for lists).
    """
    if isinstance(obj, dict):
        return tuple(sorted((k, _make_hashable(v)) for k, v in obj.items()))
    if isinstance(obj, list):
        return tuple(_make_hashable(elem) for elem in obj)
    return obj

def _normalize_result_for_set_comparison(result_data):
    """
    Normalizes LSP result data into a set of hashable representations.
    """
    if result_data is None:
        return set()
    items_to_process = result_data if isinstance(result_data, list) else [result_data]
    hashable_items = set()
    for item in items_to_process:
        if isinstance(item, dict):
            hashable_items.add(_make_hashable(item))
        else:
            logger.warning(f"Unexpected item type in list for normalization: {type(item)}. Value: {item}")
            try:
                hashable_items.add(_make_hashable(item))
            except TypeError:
                hashable_items.add(str(item))
    return hashable_items

def fetch_lsp_results(
    client_type: ClientType,
    mode: str, # "DEF" or "REF"
    lua_file_path: str,
    line: int,
    character: int,
    include_declaration: bool,
    # Paths required by different clients
    server_binary_path: Optional[str] = None,
    json_data_path: Optional[str] = None,
):
    """Fetches LSP results from the specified client."""
    if client_type == ClientType.REAL:
        if not server_binary_path:
            logger.error("Server binary path needed for REAL client.")
            return None
        if mode == "DEF":
            return get_definition_from_lua_server(server_binary_path, lua_file_path, line, character)
        elif mode == "REF":
            return get_references_from_lua_server(server_binary_path, lua_file_path, line, character, include_declaration)
    elif client_type == ClientType.MOCK:
        if not json_data_path:
            logger.error("JSON data path needed for MOCK client.")
            return None
        if mode == "DEF":
            return get_definition_from_json(json_data_path, lua_file_path, line, character)
        elif mode == "REF":
            return get_references_from_json(json_data_path, lua_file_path, line, character, include_declaration)
    elif client_type == ClientType.NAIVE:
        if mode == "DEF":
            return get_definition_from_naive_client(lua_file_path, line, character)
        elif mode == "REF":
            return get_references_from_naive_client(lua_file_path, line, character, include_declaration)
    else:
        logger.error(f"Unknown client type: {client_type}")
        return None
    return None # Should not be reached if mode is DEF/REF

def main():
    parser = argparse.ArgumentParser(description="Compare results from two different LSP client implementations.")
    parser.add_argument("line", type=int, help="Line number (0-based) to evaluate.")
    parser.add_argument("char", type=int, help="Character number (0-based) to evaluate.")
    parser.add_argument(
        "-M", "--mode", type=str, choices=["DEF", "REF"], required=True,
        help="Operation mode: 'DEF' for definitions, 'REF' for references."
    )
    parser.add_argument(
        "-A", "--client-a", type=ClientType, choices=list(ClientType), required=True,
        help="Client A (considered the 'golden standard' for this comparison)."
    )
    parser.add_argument(
        "-B", "--client-b", type=ClientType, choices=list(ClientType), required=True,
        help="Client B (the client being compared against Client A)."
    )
    parser.add_argument(
        "--include-declaration", action=argparse.BooleanOptionalAction, default=True,
        help="For 'REF' mode, whether to include the declaration in the references list (default: --include-declaration)."
    )
    # Path arguments (provide defaults)
    parser.add_argument("--server-path", type=str, default=LUA_SERVER_BINARY_PATH, help="Path to lua-language-server binary.")
    parser.add_argument("--lua-file-path", type=str, default=EVALUATION_LUA_FILE_PATH, help="Path to the Lua file to analyze.")
    parser.add_argument("--json-path", type=str, default=SYMBOL_DATA_JSON_PATH, help="Path to the symbol JSON data file.")

    args = parser.parse_args()

    if args.client_a == args.client_b:
        logger.error("Client A and Client B cannot be the same. Please choose two different clients.")
        return

    TARGET_LINE = args.line
    TARGET_CHAR = args.char
    MODE = args.mode.upper()
    INCLUDE_DECLARATION = args.include_declaration

    # Fetch results for Client A
    print(f"\n[A] Querying Client A ({args.client_a}) for {MODE}...")
    result_a = fetch_lsp_results(
        client_type=args.client_a,
        mode=MODE, 
        lua_file_path=args.lua_file_path,
        line=TARGET_LINE, 
        character=TARGET_CHAR, 
        include_declaration=INCLUDE_DECLARATION,
        server_binary_path=args.server_path,
        json_data_path=args.json_path
    )

    # Fetch results for Client B
    print(f"\n[B] Querying Client B ({args.client_b}) for {MODE}...")
    result_b = fetch_lsp_results(
        client_type=args.client_b,
        mode=MODE, 
        lua_file_path=args.lua_file_path,
        line=TARGET_LINE, 
        character=TARGET_CHAR, 
        include_declaration=INCLUDE_DECLARATION,
        server_binary_path=args.server_path,
        json_data_path=args.json_path
    )

    print(f"\n--- Client A ({args.client_a}) Result ({MODE}) ---")
    if result_a is not None:
        print(json.dumps(result_a, indent=2, sort_keys=True))
    else:
        print(f"No {MODE.lower()} found or error occurred for Client A.")
    print("-" * 28)

    print(f"\n--- Client B ({args.client_b}) Result ({MODE}) ---")
    if result_b is not None:
        print(json.dumps(result_b, indent=2, sort_keys=True))
    else:
        print(f"No {MODE.lower()} found or error occurred for Client B.")
    print("-" * 28)

    # --- Comparison Logic --- 
    print(f"\n--- Comparison (Client A: {args.client_a} vs. Client B: {args.client_b}) ---")
    
    tp, fp, fn = 0, 0, 0
    total_a, total_b = 0, 0 
    comparison_outcome_message = f"Results are DIFFERENT ({args.client_a} vs. {args.client_b})" 

    if result_a == result_b:
        comparison_outcome_message = f"Results are IDENTICAL by direct comparison ({args.client_a} vs. {args.client_b})."
        if MODE == "REF":
            set_a = _normalize_result_for_set_comparison(result_a)
            tp = len(set_a)
            fp = 0
            fn = 0
            total_a = tp 
            total_b = tp   
    else:
        try:
            set_a = _normalize_result_for_set_comparison(result_a)
            set_b = _normalize_result_for_set_comparison(result_b)

            if set_a == set_b:
                comparison_outcome_message = f"Results are IDENTICAL by set comparison ({args.client_a} vs. {args.client_b})."
                if MODE == "REF":
                    tp = len(set_a)
                    fp = 0 
                    fn = 0 
                    total_a = len(set_a)
                    total_b = len(set_b)
            else:
                comparison_outcome_message = f"Results are DIFFERENT ({args.client_a} vs. {args.client_b})" 
                
                diff_a_only = set_a - set_b
                if diff_a_only:
                    print(f"  Items only in Client A ({args.client_a}) (not in B) ({len(diff_a_only)}):")
                    for item in diff_a_only:
                        print(f"    - {str(item)[:200]}{'...' if len(str(item)) > 200 else ''}")

                diff_b_only = set_b - set_a
                if diff_b_only:
                    print(f"  Items only in Client B ({args.client_b}) (not in A) ({len(diff_b_only)}):")
                    for item in diff_b_only:
                        print(f"    - {str(item)[:200]}{'...' if len(str(item)) > 200 else ''}")
                
                if not diff_a_only and not diff_b_only and result_a != result_b:
                    print(f"  NOTE ({args.client_a} vs. {args.client_b}): Set comparison found no content differences, but direct comparison failed...")

            if MODE == "REF":
                tp = len(set_a.intersection(set_b))
                fp = len(set_b - set_a) # In B, not in A (B is system under test)
                fn = len(set_a - set_b) # In A, not in B (A is golden standard)
                total_a = len(set_a)    # Total items in Client A (Golden for this comparison)
                total_b = len(set_b)    # Total items from Client B

        except Exception as e:
            comparison_outcome_message = f"  Could not perform detailed set comparison ({args.client_a} vs. {args.client_b}) due to an error: {e}"
            logger.error(f"Error during set comparison ({args.client_a} vs. {args.client_b})", exc_info=True)
            if MODE == "REF":
                tp, fp, fn = -1, -1, -1 
                total_a, total_b = -1, -1

    print(comparison_outcome_message)

    if MODE == "REF":
        print(f"\n--- REF Statistics (Client A: {args.client_a} vs. Client B: {args.client_b}) ---")
        if tp != -1:
            print(f"True  Positives (TP): {tp}  (A items correctly found by B)")
            print(f"False Positives (FP): {fp}  (B returned, but not in A)")
            print(f"False Negatives (FN): {fn}  (In A, but B missed)")
            print()
            print(f"Total in Client A ({args.client_a} - Golden Standard): {total_a}")
            print(f"Total Returned by Client B ({args.client_b}): {total_b}")
            
            print(f"\nCalculated Statistics ({args.client_a} vs. {args.client_b}):")
            precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
            recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
            f1_score = 0.0
            if (precision + recall) > 0:
                f1_score = 2 * (precision * recall) / (precision + recall)
            
            print(f"  Precision: {precision:.2%}")
            print(f"  Recall: {recall:.2%}")
            print(f"  F1 Score: {f1_score:.2%}")
        else:
            print(f"  Statistics ({args.client_a} vs. {args.client_b}) could not be calculated due to a comparison error.")
        print("-" * 22)

    print("-" * 18) # Final separator for the whole test case

if __name__ == "__main__":
    main() 