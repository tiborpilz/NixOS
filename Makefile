.PHONY: help build generate_node_packages deploy_klaus deploy_klaus_dry_run deploy_edge homemanager switch

help: ## Show this help.
	@sed -ne '/@sed/!s/## //p' $(MAKEFILE_LIST)

build:
ifeq (home, $(filter home,$(MAKECMDGOALS)))
	nix build '.#homeConfigurations."'$(filter-out home, $(filter-out $@,$(MAKECMDGOALS)))'".activationPackage'
else ifeq (nixos, $(filter nixos,$(MAKECMDGOALS)))
	nix build .#nixosConfigurations.$(filter-out nixos, $(filter-out $@,$(MAKECMDGOALS))).config.system.build.toplevel
else
	@echo 'Build target not valid!'
endif

generate_node_packages: ## Generate nix node packages based on packages/node/node-packages.json
	nix run nixpkgs#node2nix -- -i packages/node/node-packages.json -o packages/node/node-packages.nix -16 -c /dev/null

packages/node/node-packages.nix: generate_node_packages

deploy_klaus: ## Deploy the klaus nixos configuration to 192.168.2.134 (local adress)
	nix run nixpkgs#nixos-rebuild -- switch --flake .#klaus --target-host root@192.168.2.134

deploy_klaus_dry_run:
	nix run nixpkgs#nixos-rebuild -- dry-run --flake .#klaus --target-host root@192.168.2.34

deploy_edge: ## Deploy the edge nixos configuration to the hetzner vm
	nix run nixpkgs#nixos-rebuild -- switch --flake .#edge --target-host root@159.69.194.44

homemanager: ## Swith the home-manager configuration for the current user
	home-manager switch --flake .

switch: ## Automatically switch configuration based on the system type
	@if [ -f /etc/os-release ] && grep -q "ID=nixos" /etc/os-release; then \
        @echo "NixOS detected - running nixos-rebuild"; \
		nixos-rebuild switch --flake .; \
	else \
		@echo "Non-NixOS detected - running home-manager"; \
		home-manager switch --flake .; \
	fi

%:
	@:
