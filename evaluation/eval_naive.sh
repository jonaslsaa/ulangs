#!/bin/bash

# --- Configuration ---
PYTHON_EXEC="/opt/homebrew/bin/python3.11"
SCRIPT_NAME="compare_any.py" # Changed to use the new comparison script
SRC_DIR="src"
CLIENT_A="REAL"
CLIENT_B="NAIVE"
# --- End Configuration ---

echo "======================================================================"
echo "    STARTING EVALUATION: ${CLIENT_A} LSP vs. ${CLIENT_B} Client"
echo "======================================================================"
echo

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
# Using the same test cases as the original eval.sh
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

test_cases_ref=(                                                                                                                                                    
    "GameObject (class name);3;10"
    "GameObject.new (method);3;21"
    "area (param usage in new);7;17"
    "x (field set in new);8;22"
    "timer (field usage in update);20;13"
    "shape (field usage in destroy);33;39"
    "shapes (local var usage in enter);47;12"
    "tag (param in enter);40;25"
    "HC.remove (library func call);33;27"
    "object (local var decl in enemyProjectileCollisions);52;15"               
)
# --- End Test Cases ---

# --- Modes to run ---
MODES_TO_RUN=("DEF" "REF")
# --- End Modes to run ---

for MODE_ARG in "${MODES_TO_RUN[@]}"; do
    echo "######################################################################"
    echo "          RUNNING EVALUATION FOR MODE: $MODE_ARG (${CLIENT_A} vs. ${CLIENT_B})"
    echo "######################################################################"
    echo

    mode_arg_lower=$(echo "$MODE_ARG" | tr '[:upper:]' '[:lower:]')
    current_test_cases_name="test_cases_${mode_arg_lower}[@]"
    current_test_cases=("${!current_test_cases_name}")

    total_count=${#current_test_cases[@]}

    echo "Comparing ${CLIENT_A} LSP server and ${CLIENT_B} Client results in MODE: $MODE_ARG"
    echo "Total test cases for this mode: $total_count"
    echo

    if [[ "$MODE_ARG" == "REF" ]]; then
        sum_tp=0
        sum_fp=0
        sum_fn=0
        sum_precision_val=0.0
        sum_recall_val=0.0
        sum_f1_val=0.0
        stat_producing_test_cases=0
    fi

    mode_output_file=$(mktemp)
    trap 'rm -f "$mode_output_file"' EXIT

    cd "$SRC_DIR" || exit 1

    for test_case in "${current_test_cases[@]}"; do
        IFS=';' read -r description line col <<< "$test_case"
        py_line=$((line - 1))
        py_col=$((col - 1))

        header_output="------\\n > Test Case ($MODE_ARG - ${CLIENT_A} vs. ${CLIENT_B}): $description (at $line:$col)\\n------"
        echo -e "$header_output" | tee -a "$mode_output_file" /dev/tty

        # Updated to call compare_any.py with specific clients
        individual_run_output=$("$PYTHON_EXEC" "$SCRIPT_NAME" "$py_line" "$py_col" -M "$MODE_ARG" --client-a "$CLIENT_A" --client-b "$CLIENT_B")
        
        echo "$individual_run_output" | tee -a "$mode_output_file" /dev/tty
        echo | tee -a "$mode_output_file" /dev/tty

        if [[ "$MODE_ARG" == "REF" ]]; then
            # awk patterns remain the same as compare_any.py output for these lines is consistent
            tp_val=$(echo "$individual_run_output" | awk -F': ' '/True  Positives \(TP\):/ {sub(/ .*/, "", $2); print $2}')
            fp_val=$(echo "$individual_run_output" | awk -F': ' '/False Positives \(FP\):/ {sub(/ .*/, "", $2); print $2}')
            fn_val=$(echo "$individual_run_output" | awk -F': ' '/False Negatives \(FN\):/ {sub(/ .*/, "", $2); print $2}')

            precision_perc_str=$(echo "$individual_run_output" | awk '/^  Precision:/ {sub(/%/, "", $2); print $2}')
            recall_perc_str=$(echo "$individual_run_output" | awk '/^  Recall:/ {sub(/%/, "", $2); print $2}')
            f1_perc_str=$(echo "$individual_run_output" | awk '/^  F1 Score:/ {sub(/%/, "", $3); print $3}')

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
                echo "Warning: Could not parse all statistics for test case '$description' (${CLIENT_A} vs. ${CLIENT_B}). Skipping for aggregation." >&2
            fi
        fi
    done

    cd ..
    all_mode_output=$(cat "$mode_output_file")
    rm -f "$mode_output_file"
    trap - EXIT

    # Updated grep patterns for IDENTICAL/DIFFERENT
    # Identical pattern needs to handle "by direct comparison" or "by set comparison"
    # Using ERE for this one due to .*
    identical_pattern="Results are IDENTICAL.*\\(${CLIENT_A} vs. ${CLIENT_B}\\)"
    # For Different, the string is exact. Use grep -F for fixed string matching.
    different_exact_string="Results are DIFFERENT (${CLIENT_A} vs. ${CLIENT_B})"

    identical_count=$(echo "$all_mode_output" | grep -cE "$identical_pattern")
    # Use grep -F for the different_count to ensure literal matching without regex surprises.
    different_count=$(echo "$all_mode_output" | grep -cF "$different_exact_string")
    
    echo "========================================"
    echo "      Comparison Summary for MODE: $MODE_ARG (${CLIENT_A} vs. ${CLIENT_B})"
    echo "========================================"
    echo "Total Comparisons Run: $total_count"
    echo "          Identical: $identical_count"
    echo "          Different: $different_count"
    echo "========================================"

    if [[ $((identical_count + different_count)) -ne $total_count ]]; then
        echo "Warning: The sum of IDENTICAL and DIFFERENT results does not match the total number of comparisons for MODE: $MODE_ARG (${CLIENT_A} vs. ${CLIENT_B})." >&2
        echo "This might indicate that some test cases did not print the expected 'IDENTICAL' or 'DIFFERENT' messages for this client pair, or there was an error." >&2
        echo "Looked for IDENTICAL with: grep -cE \"$identical_pattern\"" >&2
        echo "Looked for DIFFERENT with: grep -cF \"$different_exact_string\"" >&2
    fi

    if [[ "$MODE_ARG" == "REF" ]]; then
        echo
        echo "--- Overall REF Statistics (Aggregated for ${CLIENT_A} vs. ${CLIENT_B}) ---"
        if [[ $stat_producing_test_cases -gt 0 ]]; then
            echo "Micro-Averaged (based on total TP, FP, FN across $stat_producing_test_cases test cases):"
            printf "  Total TP: %d, Total FP: %d, Total FN: %d\\n" "$sum_tp" "$sum_fp" "$sum_fn"

            overall_precision=0
            if (( sum_tp + sum_fp > 0 )); then
                overall_precision=$(echo "scale=4; $sum_tp / ($sum_tp + $sum_fp)" | bc -l)
            fi
            overall_precision_percent=$(echo "$overall_precision * 100" | bc -l)
            printf "  Overall Precision: %.2f%%\\n" "$overall_precision_percent"

            overall_recall=0
            if (( sum_tp + sum_fn > 0 )); then
                overall_recall=$(echo "scale=4; $sum_tp / ($sum_tp + $sum_fn)" | bc -l)
            fi
            overall_recall_percent=$(echo "$overall_recall * 100" | bc -l)
            printf "  Overall Recall:    %.2f%%\\n" "$overall_recall_percent"
            
            overall_f1=0
            if [[ $(echo "$overall_precision + $overall_recall > 0" | bc -l) -eq 1 ]]; then
                # Avoid division by zero if precision + recall is 0
                 if (( $(echo "$overall_precision + $overall_recall == 0" | bc -l) )); then
                    overall_f1=0
                 else
                    overall_f1=$(echo "scale=4; 2 * $overall_precision * $overall_recall / ($overall_precision + $overall_recall)" | bc -l)
                 fi
            fi
            overall_f1_percent=$(echo "$overall_f1 * 100" | bc -l)
            printf "  Overall F1-Score:  %.2f%%\\n" "$overall_f1_percent"
            echo

            echo "Macro-Averaged (average of per-test-case scores from $stat_producing_test_cases test cases):"
            avg_precision=$(echo "scale=4; $sum_precision_val / $stat_producing_test_cases" | bc -l)
            avg_recall=$(echo "scale=4; $sum_recall_val / $stat_producing_test_cases" | bc -l)
            avg_f1=$(echo "scale=4; $sum_f1_val / $stat_producing_test_cases" | bc -l)

            avg_precision_percent=$(echo "$avg_precision * 100" | bc -l)
            avg_recall_percent=$(echo "$avg_recall * 100" | bc -l)
            avg_f1_percent=$(echo "$avg_f1 * 100" | bc -l)

            printf "  Average Precision: %.2f%%\\n" "$avg_precision_percent"
            printf "  Average Recall:    %.2f%%\\n" "$avg_recall_percent"
            printf "  Average F1-Score:  %.2f%%\\n" "$avg_f1_percent"
        else
            echo "  No statistics were successfully parsed from any test cases for REF mode (${CLIENT_A} vs. ${CLIENT_B})."
        fi
        echo "-------------------------------------------"
    fi

    echo
    echo "######################################################################"
    echo "          COMPLETED EVALUATION FOR MODE: $MODE_ARG (${CLIENT_A} vs. ${CLIENT_B})"
    echo "######################################################################"
    echo
    echo
done

echo "======================================================================"
echo "          ALL EVALUATION MODES COMPLETED FOR ${CLIENT_A} vs. ${CLIENT_B}"
echo "======================================================================"


