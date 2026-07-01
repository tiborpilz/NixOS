set dotenv-load
set dotenv-filename := "servers.env"

is_nixos := shell('if [ -f /etc/os-release ] && grep -q "NixOS" /etc/os-release; then echo true; else echo false; fi')

# Show this help
help:
  just --list

# Deploy a server
deploy server mode="switch":
  NIX_SSHOPTS="-p {{env_var_or_default(uppercase(server) + "_PORT", "22")}}" \
  nh os \
    {{ if mode == "dry" { "build" } else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --hostname {{server}} \
    --target-host root@{{env_var(uppercase(server))}} \
    --build-host root@{{env_var(uppercase(server))}} \
    .

# Switch the home-manager configuration
homemanager:
  nh home switch

# Automatically switch the local configuration based on the system type
switch:
  {{ if is_nixos == "true" { "nixos-rebuild switch --flake ." } else { "nh home switch ." } }}

# Generate showcase screenshots
screenshots:
  bash screenshots/generate.sh
