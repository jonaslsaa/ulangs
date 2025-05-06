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
# Format: "Description;Line;Column" (1-based line and column)

# Test cases for Definition (DEF) mode
test_cases_def=(
    "GameObject (def);3;10"
    "area (param in new);7;17"
    "GameObject (update method);19;10"
    "self (param in update);20;8"
    "timer (field);32;10"
    "dt (param in update);19;28"
    "shape (field);33;39"
    "shapes (local var);47;12"
    "shape (param in anonymous func);52;24"
    "50 (literal);13;18"
)

# Test cases for References (REF) mode
test_cases_ref=(
    "GameObject (class name);3;10"
    "area (param in new);7;17"
    "self (in update method);20;8"
    "timer (field usage);32;10"
    "x (field usage in new);8;23",
)
# --- End Test Cases ---

# --- Modes to run ---
MODES_TO_RUN=("DEF" "REF")
# --- End Modes to run ---

# Overall script separator
echo "======================================================================"
echo "          STARTING EVALUATION FOR ALL MODES"
echo "======================================================================"
echo

for MODE_ARG in "${MODES_TO_RUN[@]}"; do
    echo "######################################################################"
    echo "          RUNNING EVALUATION FOR MODE: $MODE_ARG"
    echo "######################################################################"
    echo

    # Select the appropriate test case array based on mode
    # Convert MODE_ARG to lowercase for array name construction
    mode_arg_lower=$(echo "$MODE_ARG" | tr '[:upper:]' '[:lower:]')
    current_test_cases_name="test_cases_${mode_arg_lower}[@]" # e.g., test_cases_def[@] or test_cases_ref[@]
    current_test_cases=("${!current_test_cases_name}")

    total_count=${#current_test_cases[@]}

    echo "Comparing Lua LSP server and mock JSON data in MODE: $MODE_ARG"
    echo "Total test cases for this mode: $total_count"
    echo # Add a newline for spacing

    # Initialize sums for REF mode statistics
    if [[ "$MODE_ARG" == "REF" ]]; then
        sum_tp=0
        sum_fp=0
        sum_fn=0
        sum_precision_val=0.0 # Store sum of P values (e.g., 0.50 for 50%)
        sum_recall_val=0.0    # Store sum of R values
        sum_f1_val=0.0        # Store sum of F1 values
        stat_producing_test_cases=0 # Count of test cases that produced stats
    fi

    # Temporary file to store the mode's output
    # This allows live display with tee and later capture for summary counts
    mode_output_file=$(mktemp)
    # Ensure the temp file is cleaned up on script exit
    trap 'rm -f "$mode_output_file"' EXIT

    # Change to the source directory, exit script if cd fails
    cd "$SRC_DIR" || exit 1

    # Loop through all test cases for the selected mode
    for test_case in "${current_test_cases[@]}"; do
        # Parse the test case string using IFS (Internal Field Separator)
        IFS=';' read -r description line col <<< "$test_case"

        # Adjust line and column for 0-based indexing expected by the script
        py_line=$((line - 1))
        py_col=$((col - 1)) # Column is 1-based in test case, convert to 0-based for script

        # Prepare header for this specific test
        header_output="------\n > Test Case ($MODE_ARG): $description (at $line:$col)\n------"
        # Print header to tty and append to mode_output_file
        echo -e "$header_output" | tee -a "$mode_output_file" /dev/tty

        # Execute the python script and capture its specific output for parsing
        individual_run_output=$("$PYTHON_EXEC" "$SCRIPT_NAME" "$py_line" "$py_col" -M "$MODE_ARG")
        
        # Print individual run output to tty and append to mode_output_file
        echo "$individual_run_output" | tee -a "$mode_output_file" /dev/tty
        
        # Add a blank line after each comparison's output for better readability
        echo | tee -a "$mode_output_file" /dev/tty

        # If in REF mode, parse statistics from this individual run
        # This happens in the main shell, so sum/counter variables are updated correctly
        if [[ "$MODE_ARG" == "REF" ]]; then
            # Parse TP, FP, FN (raw counts)
            tp_val=$(echo "$individual_run_output" | awk -F': ' '/True  Positives \(TP\):/ {sub(/ .*/, "", $2); print $2}')
            fp_val=$(echo "$individual_run_output" | awk -F': ' '/False Positives \(FP\):/ {sub(/ .*/, "", $2); print $2}')
            fn_val=$(echo "$individual_run_output" | awk -F': ' '/False Negatives \(FN\):/ {sub(/ .*/, "", $2); print $2}')

            # Parse Precision, Recall, F1 (percentages)
            precision_perc_str=$(echo "$individual_run_output" | awk '/^  Precision:/ {sub(/%/, "", $2); print $2}')
            recall_perc_str=$(echo "$individual_run_output" | awk '/^  Recall:/ {sub(/%/, "", $2); print $2}')
            f1_perc_str=$(echo "$individual_run_output" | awk '/^  F1 Score:/ {sub(/%/, "", $3); print $3}')

            # DEBUG: Print parsed statistic values
            echo "DEBUG: Parsed for '$description':" >&2
            echo "DEBUG:   tp_val             = '$tp_val'" >&2
            echo "DEBUG:   fp_val             = '$fp_val'" >&2
            echo "DEBUG:   fn_val             = '$fn_val'" >&2
            echo "DEBUG:   precision_perc_str = '$precision_perc_str'" >&2
            echo "DEBUG:   recall_perc_str    = '$recall_perc_str'" >&2
            echo "DEBUG:   f1_perc_str        = '$f1_perc_str'" >&2
            # END DEBUG

            if [[ -n "$tp_val" && -n "$fp_val" && -n "$fn_val" && \
                  -n "$precision_perc_str" && -n "$recall_perc_str" && -n "$f1_perc_str" ]]; then
                sum_tp=$((sum_tp + tp_val))
                sum_fp=$((sum_fp + fp_val))
                sum_fn=$((sum_fn + fn_val))

                sum_precision_val=$(echo "$sum_precision_val + ($precision_perc_str / 100)" | bc -l)
                sum_recall_val=$(echo "$sum_recall_val + ($recall_perc_str / 100)" | bc -l)
                sum_f1_val=$(echo "$sum_f1_val + ($f1_perc_str / 100)" | bc -l)
                
                stat_producing_test_cases=$((stat_producing_test_cases + 1))
            else
                echo "Warning: Could not parse all statistics for test case '$description'. Skipping for aggregation." >&2
            fi
        fi
    done

    # Go back to the original directory
    cd ..

    # Read the accumulated output from the temporary file
    all_mode_output=$(cat "$mode_output_file")
    rm -f "$mode_output_file" # Clean up the temp file
    trap - EXIT # Clear the trap now that the file is removed

    # --- Count IDENTICAL/DIFFERENT Results from all_mode_output ---
    identical_count=$(echo "$all_mode_output" | grep -o "IDENTICAL" | wc -l | tr -d ' ')
    different_count=$(echo "$all_mode_output" | grep -o "DIFFERENT" | wc -l | tr -d ' ')

    # --- Display Summary for the current mode ---
    echo "========================================"
    echo "      Comparison Summary for MODE: $MODE_ARG"
    echo "========================================"
    echo "Total Comparisons Run: $total_count"
    echo "          Identical: $identical_count"
    echo "          Different: $different_count"
    echo "========================================"

    # Optional: Check if counts add up (sanity check)
    if [[ $((identical_count + different_count)) -ne $total_count ]]; then
        echo "Warning: The sum of IDENTICAL and DIFFERENT results does not match the total number of comparisons for MODE: $MODE_ARG." >&2
        echo "This might indicate that some test cases did not print 'IDENTICAL' or 'DIFFERENT', or there was an error." >&2
    fi

    # --- Display Aggregated REF Statistics ---
    if [[ "$MODE_ARG" == "REF" ]]; then
        echo
        echo "--- Overall REF Statistics (Aggregated) ---"
        if [[ $stat_producing_test_cases -gt 0 ]]; then
            # Micro-Averaged (based on total TP, FP, FN)
            echo "Micro-Averaged (based on total TP, FP, FN across $stat_producing_test_cases test cases):"
            printf "  Total TP: %d, Total FP: %d, Total FN: %d\n" "$sum_tp" "$sum_fp" "$sum_fn"

            overall_precision=0
            if (( sum_tp + sum_fp > 0 )); then
                overall_precision=$(echo "scale=4; $sum_tp / ($sum_tp + $sum_fp)" | bc -l)
            fi
            overall_precision_percent=$(echo "$overall_precision * 100" | bc -l)
            printf "  Overall Precision: %.2f%%\n" "$overall_precision_percent"

            overall_recall=0
            if (( sum_tp + sum_fn > 0 )); then
                overall_recall=$(echo "scale=4; $sum_tp / ($sum_tp + $sum_fn)" | bc -l)
            fi
            overall_recall_percent=$(echo "$overall_recall * 100" | bc -l)
            printf "  Overall Recall:    %.2f%%\n" "$overall_recall_percent"
            
            overall_f1=0
            if [[ $(echo "$overall_precision + $overall_recall > 0" | bc -l) -eq 1 ]]; then
                overall_f1=$(echo "scale=4; 2 * $overall_precision * $overall_recall / ($overall_precision + $overall_recall)" | bc -l)
            fi
            overall_f1_percent=$(echo "$overall_f1 * 100" | bc -l)
            printf "  Overall F1-Score:  %.2f%%\n" "$overall_f1_percent"
            echo

            # Macro-Averaged (average of individual scores)
            echo "Macro-Averaged (average of per-test-case scores from $stat_producing_test_cases test cases):"
            avg_precision=$(echo "scale=4; $sum_precision_val / $stat_producing_test_cases" | bc -l)
            avg_recall=$(echo "scale=4; $sum_recall_val / $stat_producing_test_cases" | bc -l)
            avg_f1=$(echo "scale=4; $sum_f1_val / $stat_producing_test_cases" | bc -l)

            avg_precision_percent=$(echo "$avg_precision * 100" | bc -l)
            avg_recall_percent=$(echo "$avg_recall * 100" | bc -l)
            avg_f1_percent=$(echo "$avg_f1 * 100" | bc -l)

            printf "  Average Precision: %.2f%%\n" "$avg_precision_percent"
            printf "  Average Recall:    %.2f%%\n" "$avg_recall_percent"
            printf "  Average F1-Score:  %.2f%%\n" "$avg_f1_percent"
        else
            echo "  No statistics were successfully parsed from any test cases for REF mode."
        fi
        echo "-------------------------------------------"
    fi

    echo
    echo "######################################################################"
    echo "          COMPLETED EVALUATION FOR MODE: $MODE_ARG"
    echo "######################################################################"
    echo
    echo

done

echo "======================================================================"
echo "          ALL EVALUATION MODES COMPLETED"
echo "======================================================================"
