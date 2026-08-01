set dotenv-load
set dotenv-filename := "servers.env"

is_nixos := shell('if [ -f /etc/os-release ] && grep -q "NixOS" /etc/os-release; then echo true; else echo false; fi')

# Show this help
help:
  just --list

# Deploy a server
deploy server mode="switch":
  NIX_SSHOPTS="-p {{env_var_or_default(uppercase(server) + "_PORT", "22")}}" \
  nix run nixpkgs#nixos-rebuild -- \
    {{ if mode == "dry" { "dry-run"} else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --flake .#{{server}} \
    --target-host root@{{env_var(uppercase(server))}} \
    --build-host root@{{env_var(uppercase(server))}} \
    --fast

# Deploy, building locally and pushing the closure (for slow targets)
deploy-push server address="" mode="switch":
  NIX_SSHOPTS="-i ~/.ssh/id" \
  nix run nixpkgs#nixos-rebuild -- \
    {{ if mode == "dry" { "dry-run"} else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --flake .#{{server}} \
    --target-host root@{{ if address == "" { env_var(uppercase(server)) } else { address } }} \
    --fast

# Deploy a host with deploy-rs (SSH transport from ~/.ssh/config)
deploy-rs node="klaus" *ARGS:
  nix run github:serokell/deploy-rs -- .#{{node}} {{ARGS}}

# Switch the home-manager configuration
homemanager:
  nh home switch

# Automatically switch the local configuration based on the system type
switch:
  {{ if is_nixos == "true" { "sudo nixos-rebuild switch --flake ." } else { "nh home switch ." } }}

# Generate showcase screenshots
screenshots:
  bash screenshots/generate.sh
