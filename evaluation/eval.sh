#!/bin/bash

# --- Configuration ---
PYTHON_EXEC="/opt/homebrew/bin/python3.11"
SCRIPT_NAME="manual_compare.py"
SRC_DIR="src"
# --- End Configuration ---

# Check if Python executable exists
if [[ ! -x "$PYTHON_EXEC" ]]; then
    echo "Error: Python interpreter '$PYTHON_EXEC' not found or not executable." >&2
    exit 1
fi

# Check if source directory exists
if [[ ! -d "$SRC_DIR" ]]; then
    echo "Error: Source directory '$SRC_DIR' not found." >&2
    exit 1
fi

# Check if script exists within the source directory
if [[ ! -f "$SRC_DIR/$SCRIPT_NAME" ]]; then
    echo "Error: Script '$SRC_DIR/$SCRIPT_NAME' not found." >&2
    exit 1
fi

# --- Test Cases ---
# Each element: "Description;Line;Column"
test_cases=(
    "GameObject on line 3, col 10;3;10"
    "area on line 7, col 17;7;17"
    "GameObject on line 19, col 10;19;10"
    "self on line 20, col 8;20;8"
    "timer on line 32, col 10;32;10"
    "dt on line 19, col 28;19;28"
    "shape on line 33, col 39;33;39"
    "shapes on line 47, col 12;47;12"
    "shape on line 52, col 24;52;24"
    "50 (literal) on line 13, col 18;13;18"
)
# --- End Test Cases ---

output_buffer=""
total_count=${#test_cases[@]}

echo "Comparing Lua LSP server and mock JSON data..."
echo # Add a newline for spacing

# --- Execute Comparisons ---
# Use command substitution $() to capture the output of the subshell {}
# Inside the subshell, pipe (|) the output to 'tee /dev/tty'
# 'tee /dev/tty' prints the output to the current terminal AND passes it through stdout
# The outer $() captures the stdout from tee into the output_buffer variable
output_buffer=$(
    {
        # Change to the source directory, exit script if cd fails
        cd "$SRC_DIR" || exit 1

        # Loop through all test cases
        for test_case in "${test_cases[@]}"; do
            # Parse the test case string using IFS (Internal Field Separator)
            IFS=';' read -r description line col <<< "$test_case"

            # Adjust line and column for 0-based indexing expected by the script
            py_line=$((line - 1))
            py_col=$((col - 1))

            # Print the header for this specific test
            echo -e "------\n > GoTo: $description\n------" # Use -e to interpret \n

            # Execute the python script with the adjusted line and column
            "$PYTHON_EXEC" "$SCRIPT_NAME" "$py_line" "$py_col"

            # Add a blank line after each comparison's output for better readability
            echo
        done

        # Go back to the original directory
        cd ..

    } | tee /dev/tty # End of grouped commands, pipe output to tee
) # End of command substitution

# Check the exit status of the last command in the pipeline (tee)
# Note: This primarily checks tee. Errors inside the loop might need more specific checks if needed.
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
     echo "Warning: Potential error occurred during script execution within '$SRC_DIR'." >&2
     # Decide if you want to exit here based on severity
     # exit 1
fi


# --- Count Results ---
# Use grep -o to find all occurrences of the words and wc -l to count them
# The tr -d ' ' removes potential leading/trailing whitespace from wc -l output
identical_count=$(echo "$output_buffer" | grep -o "IDENTICAL" | wc -l | tr -d ' ')
different_count=$(echo "$output_buffer" | grep -o "DIFFERENT" | wc -l | tr -d ' ')

# --- Display Summary ---
echo "========================================"
echo "          Comparison Summary"
echo "========================================"
echo "Total Comparisons Run: $total_count"
echo "          Identical: $identical_count"
echo "          Different: $different_count"
echo "========================================"

# Optional: Check if counts add up (sanity check)
if [[ $((identical_count + different_count)) -ne $total_count ]]; then
    echo "Warning: The sum of IDENTICAL and DIFFERENT results does not match the total number of comparisons." >&2
fi
