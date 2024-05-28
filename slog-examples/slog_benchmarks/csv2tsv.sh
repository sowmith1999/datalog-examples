#!/bin/bash

# Directory containing the CSV files
DIR="/home/sowmith/projects/datalog-examples/slog-examples/slog_benchmarks/lubm_125/db"

# Loop through each CSV file in the directory
for FILE in "$DIR"/*.csv; do
  # Replace commas with tabs and save to a temporary file
  sed 's/,/\t/g' "$FILE" > "${FILE}.tmp"
  # Replace the original file with the modified one
  mv "${FILE}.tmp" "$FILE"
done

