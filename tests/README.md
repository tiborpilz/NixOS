# NixOS Tests

This directory contains integration tests for the NixOS and Home Manager configurations.

## Tests

### home-manager.nix

A comprehensive test suite for the Home Manager configuration that validates both functional and visual aspects of the user environment.

#### Test Objectives

1. **Isolation**: Tests run in a clean, isolated Virtual Machine (VM) to ensure reproducibility
2. **Functional Verification**: Verifies the presence and execution of key user packages (e.g., `git`, `zsh`, `direnv`, `nvim`)
3. **Visual Verification**: Validates "Look & Feel" elements, specifically Neovim theming and status line configurations using screenshots

#### Test Architecture

- **Framework**: NixOS Integration Testing framework (`nixos-test`)
- **VM Configuration**: 
  - X11 enabled for graphical testing
  - i3 window manager (minimal footprint)
  - Test user: `testuser` with zsh shell
- **Home Manager**: Full home configuration imported with test-specific overrides

#### Running the Test

```bash
# Run the home-manager test
nix build .#checks.x86_64-linux.home-manager-test

# Or run all checks
nix flake check
```

#### Test Coverage

**Functional Tests:**
- ✓ Neovim (`nvim`) is in `$PATH` and runs without errors
- ✓ Git is in `$PATH` and configured
- ✓ Direnv is available
- ✓ Zsh is the user shell
- ✓ Neovim configuration files exist (`~/.config/nvim`)

**Visual Tests:**
- ✓ X11 environment starts successfully
- ✓ Terminal emulator (xterm) launches
- ✓ Neovim opens in graphical terminal
- ✓ Screenshot capture of Neovim startup state

#### Limitations and Notes

1. **Neovim Config Override**: The test overrides the out-of-store symlink configuration for Neovim to use files directly from the Nix store, as the test VM doesn't have access to the user's code directory.

2. **OCR Tests**: While the test framework supports OCR-based text detection (e.g., `machine.wait_for_text()`), these checks are commented out as they can be unreliable. Visual verification primarily relies on screenshots and functional tests.

3. **Syncthing Service**: Disabled in tests to reduce complexity and resource usage.

4. **Graphical Mode**: Enabled to allow visual testing of terminal applications, though this increases test runtime.

### paperless.nix

Tests the Paperless-ngx service configuration using Podman containers.

### tandoor.nix

Tests the Tandoor Recipes service configuration using Podman containers.
