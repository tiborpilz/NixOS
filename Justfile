set dotenv-load

is_nixos := shell('if [ -f /etc/os-release ] && grep -q "NixOS" /etc/os-release; then echo true; else echo false; fi')

# Show this help
help:
  just --list

# Deploy a server
deploy server mode="switch":
  nix run nixpkgs#nixos-rebuild -- \
    {{ if mode == "dry" { "dry-run"} else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --flake .#{{server}} \
    --target-host root@{{env_var(uppercase(server))}} \

# Switch the home-manager configuration
homemanager:
  nh home switch

# Automatically switch the local configuration based on the system type
switch:
  {{ if is_nixos == "true" { "nixos-rebuild switch --flake ." } else { "home-manager switch --flake ." } }}
