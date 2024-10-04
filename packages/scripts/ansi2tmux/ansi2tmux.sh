#!/usr/bin/env bash

# Function to convert ANSI color codes to Tmux format
convert_ansi_to_tmux() {
    # Get all arguments
    local input="$@"

    # Replace ANSI escape codes with corresponding Tmux status formatting
    output=$(echo "$input" | sed -E \
        -e 's/\x1b\[1?;?3([1-6])m/#[fg=colour\1]/g' \
        -e 's/\x1b\[0m/#[default]/g')

    echo "$output"
}

convert_ansi_to_tmux "$@"
