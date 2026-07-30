{ pkgs, ... }:

# Claude Desktop. Anthropic ships an official Linux build (beta) as a .deb
# through their own apt repo; we repackage that directly instead of using the
# community flake, which repacks the *Windows* build and lags far behind.
# See packages/claude-desktop/claude-desktop.nix for the details.
{
  claude-desktop = pkgs.callPackage ./claude-desktop.nix { };
}
