import os
import re
import logging
from pathlib import Path
from typing import Optional, Union, List, Dict, Any, Tuple

# Configure logging
# Use INFO for detailed operational messages, WARNING for issues, ERROR for failures.
# logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
# To see output from this logger, ensure the calling script (e.g., manual_compare.py)
# or the environment configures logging to include messages from this logger name
# and at the desired level. For now, we'll rely on print for direct output if needed in testing.

def _create_lsp_range(
    line: int,          # 0-based
    start_char: int,    # 0-based
    end_char: int       # 0-based, exclusive
) -> Dict[str, Any]:
    """Creates an LSP Range dictionary."""
    return {
        "start": {"line": line, "character": start_char},
        "end": {"line": line, "character": end_char}
    }

def _extract_token_at_position(
    file_lines: List[str],
    line_idx: int,      # 0-based
    char_idx: int       # 0-based
) -> Optional[Tuple[str, Dict[str, Any]]]: # (token_string, origin_selection_range_dict)
    """
    Extracts a Lua identifier token from the given line at the character position.
    Returns the token string and its LSP Range if found, otherwise None.
    A Lua identifier is typically [a-zA-Z_][a-zA-Z0-9_]*.
    """
    if not (0 <= line_idx < len(file_lines)):
        logger.warning(f"Line index {line_idx} is out of bounds.")
        return None

    line_content = file_lines[line_idx]
    
    # Regex to find Lua-like identifiers
    # \b ensures whole word matching.
    identifier_regex = r"\b([a-zA-Z_][a-zA-Z0-9_]*)\b"
    
    for match in re.finditer(identifier_regex, line_content):
        token = match.group(1)
        start_char = match.start(1)
        end_char = match.end(1)
        
        # Check if the char_idx is within this token's span
        # For a cursor at char_idx, it can be on any character of the token,
        # or just after the last character of the token if it's at the boundary.
        # If char_idx is exactly at start_char, it's on the token.
        # If char_idx is between start_char and end_char-1, it's on the token.
        # If char_idx is at end_char, some editors might consider it part of the token to the left.
        # Let's consider char_idx >= start_char and char_idx < end_char
        # OR if char_idx == end_char AND start_char < end_char (token has length)
        # A common behavior is that if cursor is at `end_char`, it refers to token if `char_idx == match.start()` or `match.start() < char_idx <= match.end()`.
        # Let's try: if cursor is at `abc|`, `char_idx` is 3. `start` is 0, `end` is 3. `0 <= 3 < 3` is false.
        # Need to include the case where char_idx is at the end of the token or immediately after.
        # A simpler check: does the span [match.start(), match.end()) contain char_idx?
        # Or, if char_idx is immediately at the boundary, which token to pick?
        # Let's use: char_idx is within [start, end). If char_idx == end, it's typically for the char *after* token.
        
        # If cursor is at `tok|en`, char_idx points to `e`.
        # If `token` starts at 5, `char_idx` must be 5, 6, 7.
        # `re.match(pattern, string, pos)` uses `pos` as start.
        # Let's find a token that *contains* the char_idx or starts at char_idx.
        if start_char <= char_idx < end_char:
            origin_range = _create_lsp_range(line_idx, start_char, end_char)
            logger.info(f"Extracted token '{token}' at {line_idx}:{start_char}-{end_char}")
            return token, origin_range
        # Case: cursor is immediately to the left of a token (e.g. |token)
        # if char_idx == start_char: (already covered by above)

    # If no token found directly at char_idx, check if char_idx is adjacent to a token.
    # This can be complex. For now, strict containment or char_idx = start_char.
    # The above loop with `start_char <= char_idx < end_char` is standard.
    # If char_idx is at the end of a word (e.g. word|), it means char_idx == end_char.
    # Let's adjust: if char_idx is at the end of a token, we still consider it that token.
    # Example: "token", char_idx = 5 (0-indexed), start=0, end=5. We want to match.
    # So, if char_idx == end_char and start_char < end_char, it's a valid selection.
    for match in re.finditer(identifier_regex, line_content):
        token = match.group(1)
        start_char = match.start(1) # start of the token itself
        end_char = match.end(1)   # end of the token itself (exclusive)

        # If char_idx is within the token [start, end-1]
        # or if char_idx is exactly at the end of the token, consider it selected
        if (start_char <= char_idx < end_char) or (char_idx == end_char and start_char < char_idx):
            origin_range = _create_lsp_range(line_idx, start_char, end_char)
            # logger.info(f"Extracted token '{token}' at {line_idx}:{start_char}-{end_char} (char_idx: {char_idx})")
            return token, origin_range


    logger.warning(f"No token found at {line_idx}:{char_idx} in line: '{line_content}'")
    return None

def get_definition_from_naive_client(
    lua_file_path: str,
    line: int, # 0-based
    character: int # 0-based
) -> Optional[List[Dict[str, Any]]]: # Returns list with one LocationLink-like dict or None
    """
    Naively finds the 'definition' of a token.
    Extracts token at position, then returns the first occurrence in the file.
    """
    if not os.path.exists(lua_file_path):
        logger.error(f"Lua file not found: {lua_file_path}")
        return None

    try:
        with open(lua_file_path, 'r', encoding='utf-8') as f:
            file_content = f.read()
            file_lines = file_content.splitlines() # Keep for _extract_token_at_position
    except Exception as e:
        logger.error(f"Error reading Lua file {lua_file_path}: {e}")
        return None

    token_info = _extract_token_at_position(file_lines, line, character)
    if not token_info:
        logger.info(f"Could not extract token from {lua_file_path} at {line}:{character}")
        return None
    
    token_to_find, origin_selection_range = token_info
    logger.info(f"Looking for definition of token: '{token_to_find}'")

    # Search for the first occurrence of the token (whole word)
    # Pattern for whole word search: \btoken\b
    # Need to escape the token if it contains regex special characters (unlikely for identifiers)
    search_pattern = r"\b" + re.escape(token_to_find) + r"\b"
    
    first_occurrence_range = None
    found_line_idx = -1
    
    for i, current_line_content in enumerate(file_lines):
        for match in re.finditer(search_pattern, current_line_content):
            start_char = match.start()
            end_char = match.end()
            # This is the first one found
            target_range = _create_lsp_range(i, start_char, end_char)
            target_selection_range = target_range # For simplicity
            
            file_uri = Path(lua_file_path).as_uri()
            
            location_link = {
                "originSelectionRange": origin_selection_range,
                "targetUri": file_uri,
                "targetRange": target_range,
                "targetSelectionRange": target_selection_range 
                # No "targetName" or complex parts, just basic LocationLink structure
            }
            logger.info(f"Naive definition found for '{token_to_find}' at {i}:{start_char}")
            return [location_link] # Return as a list containing one item

    logger.info(f"Token '{token_to_find}' not found in {lua_file_path} for definition.")
    return None


def get_references_from_naive_client(
    lua_file_path: str,
    line: int, # 0-based
    character: int, # 0-based
    include_declaration: bool = True # Naive client implicitly includes declaration if found
) -> Optional[List[Dict[str, Any]]]: # Returns list of Location dicts or None
    """
    Naively finds all 'references' of a token.
    Extracts token at position, then returns all occurrences in the file.
    """
    if not os.path.exists(lua_file_path):
        logger.error(f"Lua file not found: {lua_file_path}")
        return None

    try:
        with open(lua_file_path, 'r', encoding='utf-8') as f:
            file_content = f.read()
            file_lines = file_content.splitlines()
    except Exception as e:
        logger.error(f"Error reading Lua file {lua_file_path}: {e}")
        return None

    token_info = _extract_token_at_position(file_lines, line, character)
    if not token_info:
        logger.info(f"Could not extract token from {lua_file_path} at {line}:{character} for references")
        return None # Return None if no token to search for
    
    token_to_find, _ = token_info # We don't need origin_selection_range for the list of references
    logger.info(f"Looking for all references of token: '{token_to_find}'")

    references: List[Dict[str, Any]] = []
    file_uri = Path(lua_file_path).as_uri()
    
    search_pattern = r"\b" + re.escape(token_to_find) + r"\b"

    for i, current_line_content in enumerate(file_lines):
        for match in re.finditer(search_pattern, current_line_content):
            start_char = match.start()
            end_char = match.end()
            
            lsp_range = _create_lsp_range(i, start_char, end_char)
            location = {
                "uri": file_uri,
                "range": lsp_range
            }
            references.append(location)
            
    if not references:
        logger.info(f"Token '{token_to_find}' not found in {lua_file_path} for references.")
        return None # As per mock_lsp_client, return None if nothing found. Or empty list?
                     # manual_compare normalizes None to set(), so None is fine.
    
    logger.info(f"Found {len(references)} naive references for '{token_to_find}'.")
    return references

# --- Example Usage (for testing this file directly) ---
if __name__ == "__main__":
    # Configure basic logging for direct script testing
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    logger.info("Running naive_lsp_client.py directly for testing with evaluation-sample.lua.")

    # Hardcoded path to the actual evaluation sample file
    actual_lua_file_path = "/Users/jonassilva/Desktop/uLangs/evaluation/data/evaluation-sample.lua"
    
    if not os.path.exists(actual_lua_file_path):
        logger.error(f"Test file not found: {actual_lua_file_path}. Please check the path.")
        exit(1)

    # Import json here as it's only used in this test block
    import json 

    # Test 1: Definition of 'GameObject' 
    # GameObject = Object:extend() (line 1, col 0 for 'G' -> 0-indexed line 0, char 0)
    logger.info("\n--- Test Case 1: Definition of GameObject ---")
    def_result_go = get_definition_from_naive_client(actual_lua_file_path, 0, 0)
    if def_result_go:
        logger.info(f"Definition Result (GameObject@0:0): {json.dumps(def_result_go, indent=2)}")
    else:
        logger.info("No definition found for GameObject@0:0")

    # Test 2: References of 'timer'
    # Click on 'self.timer' in 'GameObject:update(dt)' function: self.timer:update(dt) 
    # (line 20, col 12 for 't' in 'timer' -> 0-indexed line 19, char 12)
    logger.info("\n--- Test Case 2: References of timer ---")
    ref_result_timer = get_references_from_naive_client(actual_lua_file_path, 19, 12)
    if ref_result_timer:
        logger.info(f"References Result (timer@19:12): {json.dumps(ref_result_timer, indent=2)}")
    else:
        logger.info("No references found for timer@19:12")

    # Test 3: Token extraction for 'dt'
    # In 'GameObject:update(dt)' (line 19, col 27 for 'd' in 'dt' -> 0-indexed line 18, char 27)
    logger.info("\n--- Test Case 3: Token Extraction for dt ---")
    # Need to read file lines for _extract_token_at_position
    try:
        with open(actual_lua_file_path, 'r', encoding='utf-8') as f:
            file_lines_for_test = f.read().splitlines()
        token_info_dt = _extract_token_at_position(file_lines_for_test, 18, 27) 
        if token_info_dt:
            logger.info(f"Token extraction (dt@18:27): Token='{token_info_dt[0]}', Range={json.dumps(token_info_dt[1], indent=2)}")
        else:
            logger.info("Token extraction failed for dt@18:27")
    except Exception as e:
        logger.error(f"Failed to read file for Test Case 3: {e}")

    # Test 4: No token under cursor (e.g. whitespace)
    # Example: line 2, char 0 (empty line) -> 0-indexed line 1, char 0
    logger.info("\n--- Test Case 4: No token under cursor (whitespace/empty line) ---")
    def_no_token = get_definition_from_naive_client(actual_lua_file_path, 1, 0) # Empty line
    if def_no_token:
        logger.info(f"Definition Result (no_token@1:0): {json.dumps(def_no_token, indent=2)}")
    else:
        logger.info("No definition found for no_token@1:0 (expected as it's an empty line)")

    # Test 5: Cursor at end of token 'self.area' in GameObject:new 
    # self.area = area (line 7, 'area' ends at char 13. Cursor at 13 -> 0-indexed line 6, char 13)
    logger.info("\n--- Test Case 5: Cursor at end of token 'area' ---") 
    def_result_end_area = get_definition_from_naive_client(actual_lua_file_path, 6, 13)
    if def_result_end_area:
        logger.info(f"Definition Result (area@6:13): {json.dumps(def_result_end_area, indent=2)}")
    else:
        logger.info("No definition found for area@6:13")

    logger.info("\n--- Naive client direct testing complete ---")

# Removed tests 7-10 as they were related to include_declaration which is implicit here
# and other dummy file specifics. 