#!/bin/bash
set -e

# Display number to use for Xvfb
DISPLAY_NUM=99

# Screen dimensions
SCREEN_WIDTH=1920
SCREEN_HEIGHT=1080

# Output directory
OUTPUT_DIR="/output"

echo ${OUTPUT_DIR}

# Start Xvfb with 24-bit color depth
echo "Starting Xvfb on display :$DISPLAY_NUM"
Xvfb :$DISPLAY_NUM -screen 0 ${SCREEN_WIDTH}x${SCREEN_HEIGHT}x24 -ac &
XVFB_PID=$!

# Wait for Xvfb to start
sleep 2

echo ${DISPLAY_NUM}

# Set DISPLAY variable for X11
export DISPLAY=:$DISPLAY_NUM

# Create output directory if it doesn't exist
mkdir -p $OUTPUT_DIR

# Ensure Lazy.nvim is installed (plugin manager)
if [ ! -d "/home/neovim-user/.local/share/nvim/lazy" ]; then
    echo "Installing Lazy.nvim plugin manager..."
    git clone --filter=blob:none https://github.com/folke/lazy.nvim.git --branch=stable ~/.local/share/nvim/lazy/lazy.nvim
fi

# Function to take a screenshot
take_screenshot() {
    local name=$1
    local delay=${2:-3}  # Default delay of 3 seconds
    local commands=${3:-""}  # Optional commands to execute before screenshot
    
    echo "Launching Neovim for screenshot: $name"
    
    # If commands are provided, create a temporary file
    if [ -n "$commands" ]; then
        echo "#!/bin/bash" > /tmp/nvim_commands.sh
        echo "sleep 1" >> /tmp/nvim_commands.sh
        echo "$commands" >> /tmp/nvim_commands.sh
        chmod +x /tmp/nvim_commands.sh
        
        # Start nvim in the background and execute commands
        NVIM_APPNAME=nvim nvim --clean -c "lua vim.o.laststatus=3" &
        NVIM_PID=$!
        sleep 1  # Wait for nvim to start
        /tmp/nvim_commands.sh
    else
        # Just start nvim
        NVIM_APPNAME=nvim nvim --clean -c "lua vim.o.laststatus=3" &
        NVIM_PID=$!
    fi
    
    # Wait for specified delay
    echo "Waiting $delay seconds before capturing screenshot..."
    sleep $delay
    
    # Take the screenshot
    echo "Capturing screenshot: $name"
    scrot -o "$OUTPUT_DIR/$name.png"
    
    # Kill neovim
    if [ -n "$NVIM_PID" ]; then
        kill $NVIM_PID || true
        wait $NVIM_PID 2>/dev/null || true
        sleep 1
    fi
}

# Main program
echo "Starting screenshot generation process..."

# Take a screenshot of the default screen
take_screenshot "neovim_default" 5

# Take a screenshot with NERDTree open (if installed)
take_screenshot "neovim_file_explorer" 5 ":Explore\n"

# Take a screenshot with some code open
take_screenshot "neovim_code" 5 ":e /home/neovim-user/.config/nvim/init.lua\n"

# Add more screenshots as needed

# Cleanup
echo "Cleaning up..."
if [ -n "$XVFB_PID" ]; then
    kill $XVFB_PID || true
    wait $XVFB_PID 2>/dev/null || true
fi

echo "All screenshots have been saved to $OUTPUT_DIR" 
