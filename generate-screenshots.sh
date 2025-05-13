#!/bin/bash
set -e

# Directory to save screenshots
OUTPUT_DIR="./screenshots"

# Create output directory if it doesn't exist
mkdir -p $OUTPUT_DIR

# Build the Docker image
echo "Building Docker image..."
docker build -t neovim-screenshot -f Dockerfile.screenshot .

# Run the container to generate screenshots
echo "Generating screenshots..."
docker run --rm \
  -v "$(pwd)/$OUTPUT_DIR:/output" \
  neovim-screenshot

echo "Screenshots have been saved to $OUTPUT_DIR"
echo "Generated files:"
ls -l $OUTPUT_DIR 