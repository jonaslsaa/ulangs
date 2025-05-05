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
    line: int, # 0-based line
    character: int, # 0-based character (column)
) -> Optional[Union[Dict[str, Any], List[Dict[str, Any]]]]:
    """
    Finds the definition location for a symbol at a given position
    by looking it up in a pre-defined JSON data structure and returns
    it in a format mimicking LSP LocationLink, attempting to replicate
    observed server behavior where the origin range might be returned as target.

    Args:
        json_data_path: Absolute path to the JSON file containing symbol data.
        source_file_path: Absolute path to the source file being analyzed
                          (used to resolve "__file__" and create URIs).
        line: 0-based line number of the symbol usage/reference.
        character: 0-based character number (column) of the symbol usage/reference.

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
            loc_line = loc.get("line")
            loc_col = loc.get("col")
            loc_len = loc.get("len")
            # Assuming 0-based line/col in JSON
            if (loc_line == line and loc_col is not None and loc_len is not None and
                    character >= loc_col and character < (loc_col + loc_len)):
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

            ref_line = ref.get("line")
            ref_col = ref.get("col")
            ref_len = ref.get("len")
            # Assuming 0-based line/col in JSON
            if (ref_line == line and ref_col is not None and ref_len is not None and
                    character >= ref_col and character < (ref_col + ref_len)):
                # Found the symbol via one of its references
                found_symbol_definition_loc = symbol.get("location") # Store the actual definition loc
                origin_selection_range_data = ref # Store the range of the reference clicked
                break # Found the symbol

        if found_symbol_definition_loc:
            break # Exit outer loop once symbol is found

    # If a symbol was found at the cursor position, format its location as LocationLink
    if (found_symbol_definition_loc and isinstance(found_symbol_definition_loc, dict) and
        origin_selection_range_data and isinstance(origin_selection_range_data, dict)):

        # --- Get range data for the ORIGIN (the clicked symbol) ---
        origin_line = origin_selection_range_data.get("line")
        origin_col = origin_selection_range_data.get("col")
        origin_len = origin_selection_range_data.get("len")

        # --- Get range data for the DEFINITION (where symbol is defined) ---
        # (We still need this to check validity, even if not used for targetRange)
        def_line = found_symbol_definition_loc.get("line")
        def_col = found_symbol_definition_loc.get("col")
        def_len = found_symbol_definition_loc.get("len")


        if (def_line is not None and def_col is not None and def_len is not None and # Check definition data validity
            origin_line is not None and origin_col is not None and origin_len is not None): # Check origin data validity

            # Assuming 0-based line/col in JSON
            origin_end_col = origin_col + origin_len

            # *** Modification: Use ORIGIN range for targetRange and targetSelectionRange ***
            # This replicates the behavior observed in the real LSP server output provided,
            # where the target range matched the origin range.
            target_start_line = origin_line
            target_start_char = origin_col
            target_end_line = origin_line
            target_end_char = origin_end_col

            # Construct the LSP LocationLink dictionary format
            location_link_dict = {
                "targetUri": source_file_uri,
                "targetRange": {
                    "start": {"line": target_start_line, "character": target_start_char},
                    "end": {"line": target_end_line, "character": target_end_char}
                },
                "targetSelectionRange": {
                    "start": {"line": target_start_line, "character": target_start_char},
                    "end": {"line": target_end_line, "character": target_end_char}
                },
                "originSelectionRange": {
                    "start": {"line": origin_line, "character": origin_col},
                    "end": {"line": origin_line, "character": origin_end_col}
                }
            }
            # Return as a list containing the single location link
            return [location_link_dict]
        else:
            logger.warning(f"Found symbol but its location or origin data is incomplete: Def={found_symbol_definition_loc}, Origin={origin_selection_range_data}")
            return None
    else:
        # No symbol found at the specified line and character
        logger.info(f"No symbol found at {source_file_path} L{line}:C{character} in {json_data_path}")
        return None
