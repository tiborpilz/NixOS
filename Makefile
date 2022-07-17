deploy_homeserver:
	nix run nixpkgs#nixos-rebuild -- switch --flake .#homeserver --target-host root@192.168.2.65 --build-host localhost
