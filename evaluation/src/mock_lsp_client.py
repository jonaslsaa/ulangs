import json
import os
import logging
from pathlib import Path
from typing import Optional, Union, List, Dict, Any

# Configure logging
logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

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

    for symbol in data["symbols"]:
        # Check if the symbol structure is valid
        if not isinstance(symbol, dict) or "location" not in symbol or "references" not in symbol:
            logger.warning(f"Skipping invalid symbol entry: {symbol}")
            continue

        # Check if the cursor position falls within the definition location itself
        loc = symbol.get("location")
        if isinstance(loc, dict):
            loc_line_1based = loc.get("line") # JSON line is 1-based
            loc_col_0based = loc.get("col")   # JSON col is 0-based
            loc_len = loc.get("len")

            # Compare input (0-based) with JSON data (adjusting line)
            if (loc_line_1based is not None and loc_col_0based is not None and loc_len is not None and
                    (loc_line_1based - 1) == line and # Adjust JSON line to 0-based for comparison
                    character >= loc_col_0based and character < (loc_col_0based + loc_len)):
                found_symbol_definition_loc = loc # Store the actual definition loc
                origin_selection_range_data = loc # Clicked on the definition itself
                break # Found the symbol

        # Check if the cursor position falls within any reference location
        refs = symbol.get("references", [])
        if not isinstance(refs, list):
             logger.warning(f"Invalid references format for symbol {symbol.get('name', 'N/A')}: {refs}")
             continue

        for ref in refs:
            if not isinstance(ref, dict):
                logger.warning(f"Skipping invalid reference entry: {ref}")
                continue

            ref_line_1based = ref.get("line") # JSON line is 1-based
            ref_col_0based = ref.get("col")   # JSON col is 0-based
            ref_len = ref.get("len")

            # Compare input (0-based) with JSON data (adjusting line)
            if (ref_line_1based is not None and ref_col_0based is not None and ref_len is not None and
                    (ref_line_1based - 1) == line and
                    character >= ref_col_0based and character < (ref_col_0based + ref_len)):
                # Found the symbol via one of its references
                found_symbol_definition_loc = symbol.get("location") # Store the actual definition loc
                origin_selection_range_data = ref # Store the range of the reference clicked
                break # Found the symbol

        if found_symbol_definition_loc:
            break # Exit outer loop once symbol is found

    # If a symbol was found at the cursor position, format its location as LocationLink
    if (found_symbol_definition_loc and isinstance(found_symbol_definition_loc, dict) and
        origin_selection_range_data and isinstance(origin_selection_range_data, dict)):

        # --- Get range data for the ORIGIN (the clicked symbol) from JSON ---
        origin_line_1based = origin_selection_range_data.get("line")
        origin_col_0based = origin_selection_range_data.get("col")
        origin_len = origin_selection_range_data.get("len")

        # --- Get range data for the DEFINITION (where symbol is defined) from JSON ---
        def_line_1based = found_symbol_definition_loc.get("line")
        def_col_0based = found_symbol_definition_loc.get("col")
        def_len = found_symbol_definition_loc.get("len")


        if (def_line_1based is not None and def_col_0based is not None and def_len is not None and # Check definition data validity
            origin_line_1based is not None and origin_col_0based is not None and origin_len is not None): # Check origin data validity

            # --- Calculate LSP 0-based coordinates for the ORIGIN ---
            lsp_origin_start_line = origin_line_1based - 1
            lsp_origin_start_char = origin_col_0based
            lsp_origin_end_line = origin_line_1based - 1 # Assuming origin is single line
            lsp_origin_end_char = origin_col_0based + origin_len

            # --- Calculate LSP 0-based coordinates for the TARGET (Definition) ---
            # Use the definition's location data (def_...)
            lsp_target_start_line = def_line_1based - 1
            lsp_target_start_char = def_col_0based
            lsp_target_end_line = def_line_1based - 1 # Assuming definition is single line
            lsp_target_end_char = def_col_0based + def_len

            # Construct the LSP LocationLink dictionary format (using 0-based indexing)
            # Target ranges now correctly point to the definition location.
            location_link_dict = {
                "targetUri": source_file_uri, # Assuming definition is in the same file for now
                "targetRange": {
                    "start": {"line": lsp_target_start_line, "character": lsp_target_start_char},
                    "end": {"line": lsp_target_end_line, "character": lsp_target_end_char}
                },
                "targetSelectionRange": { # Often the same as targetRange, but could be just the identifier
                    "start": {"line": lsp_target_start_line, "character": lsp_target_start_char},
                    "end": {"line": lsp_target_end_line, "character": lsp_target_end_char}
                },
                "originSelectionRange": {
                    "start": {"line": lsp_origin_start_line, "character": lsp_origin_start_char},
                    "end": {"line": lsp_origin_end_line, "character": lsp_origin_end_char}
                }
            }
            # Return as a list containing the single location link
            return [location_link_dict]
        else:
            logger.warning(f"Found symbol but its location or origin data is incomplete: Def={found_symbol_definition_loc}, Origin={origin_selection_range_data}")
            return None
    else:
        # No symbol found at the specified line and character
        logger.info(f"No symbol found at {source_file_path} L{line}:C{character} (0-based) in {json_data_path}")
        return None
