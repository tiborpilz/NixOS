set dotenv-load

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
