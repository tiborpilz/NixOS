# Neovim Screenshot Generation

This repository includes a Docker-based solution for generating screenshots of Neovim with the configured setup. This is useful for documentation, showcasing your setup on the web, or visual regression testing.

## How It Works

The solution uses:
- Docker container with Ubuntu
- Xvfb (X Virtual Framebuffer) to provide a headless X server
- The NixOS Neovim configuration
- Scripts to automate opening specific views and capturing screenshots

## Generating Screenshots Locally

To generate screenshots on your local machine:

1. Ensure Docker is installed and running
2. Run the provided script:
   ```bash
   ./generate-screenshots.sh
   ```
3. Screenshots will be saved to the `./screenshots` directory

## Customizing Screenshots

To customize which screenshots are generated, edit the `screenshot.sh` file. The script includes a `take_screenshot` function that accepts:

- Screenshot name
- Delay before capture (in seconds)
- Optional Neovim commands to execute before capturing

Example of adding a new screenshot:

```bash
# Take a screenshot of Neovim with a specific plugin view
take_screenshot "neovim_telescope" 5 ":Telescope find_files\n"
```

## CI Integration

This repository includes a GitHub Actions workflow that automatically generates screenshots when the Neovim configuration changes. The workflow:

1. Builds the Docker image
2. Runs the screenshot generation
3. Uploads the screenshots as artifacts
4. Optionally commits the screenshots back to the repository

You can also manually trigger the workflow from the GitHub Actions tab.

## Troubleshooting

If you encounter issues:

1. **Missing dependencies**: Check the Dockerfile to ensure all necessary dependencies are installed
2. **Plugin loading issues**: Ensure all plugins can be loaded in a clean environment
3. **Timing issues**: Adjust the delay parameters in the `take_screenshot` function calls if screens are captured before Neovim is fully ready 