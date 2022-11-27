.PHONY: help

help: ## Show this help.
	@sed -ne '/@sed/!s/## //p' $(MAKEFILE_LIST)

generate_node_packages: ## Generate nix node packages based on packages/node/node-packages.json
	nix run nixpkgs#node2nix -- -i packages/node/node-packages.json -o packages/node/node-packages.nix -14

packages/node/node-packages.nix: generate_node_packages

deploy_homeserver: ## Deploy the homeserver nixos configuration to 192.168.2.68 (local adress)
	nix run nixpkgs#nixos-rebuild -- switch --flake .#homeserver --target-host root@192.168.2.68 --build-host localhost

deploy_edge: ## Deploy the edge nixos configuration to the hetzner vm
	nix run nixpkgs#nixos-rebuild -- switch --flake .#edge --target-host root@159.69.194.44 --build-host localhost

homemanager: ## Swith the home-manager configuration for the user 'tibor'
	home-manager switch --flake .#tibor
