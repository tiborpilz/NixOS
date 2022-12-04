.PHONY: help build

help: ## Show this help.
	@sed -ne '/@sed/!s/## //p' $(MAKEFILE_LIST)

build:
ifeq (home, $(filter home,$(MAKECMDGOALS)))
	nix build .#homeConfigurations.$(filter-out home, $(filter-out $@,$(MAKECMDGOALS))).activationPackage
else ifeq (nixos, $(filter nixos,$(MAKECMDGOALS)))
	nix build .#nixosConfigurations.$(filter-out nixos, $(filter-out $@,$(MAKECMDGOALS))).config.system.build.toplevel
else
	@echo 'Build target not valid!'
endif

generate_node_packages: ## Generate nix node packages based on packages/node/node-packages.json
	nix run nixpkgs#node2nix -- -i packages/node/node-packages.json -o packages/node/node-packages.nix -14

packages/node/node-packages.nix: generate_node_packages

deploy_homeserver: ## Deploy the homeserver nixos configuration to 192.168.2.68 (local adress)
	nix run nixpkgs#nixos-rebuild -- switch --flake .#homeserver --target-host root@192.168.2.68 --build-host localhost

deploy_homeserver_dry_run:
	nix run nixpkgs#nixos-rebuild -- dry-run --flake .#homeserver --target-host root@192.168.2.68 --build-host localhost

deploy_edge: ## Deploy the edge nixos configuration to the hetzner vm
	nix run nixpkgs#nixos-rebuild -- switch --flake .#edge --target-host root@159.69.194.44 --build-host localhost

homemanager: ## Swith the home-manager configuration for the current user
	home-manager switch --flake .

%:
	@:
