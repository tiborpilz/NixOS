deploy_homeserver:
	nix run nixpkgs#nixos-rebuild -- switch --flake .#homeserver --target-host root@192.168.2.68 --build-host localhost

deploy_edge:
	nix run nixpkgs#nixos-rebuild -- switch --flake .#edge --target-host root@159.69.194.44 --build-host localhost
