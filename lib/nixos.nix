{ inputs, pkgs, lib, ... }:

with lib;
with lib.my;
let sys = "x86_64-linux";
in {
  mkHost = path: attrs @ { system ? sys, ... }:
    nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs system; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)

        inputs.home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.tibor.imports = [
            ../home
          ];
        }
        ../.   # /default.nix
        # ({ ... }: {
        #   home-manager.users.tibor = import ../home { inherit lib pkgs inputs; };
        # })
        # (builtins.toPath "${inputs.nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
        # (builtins.toPath "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
        (import path)
      ];
    };

  mapHosts = dir: attrs @ { system ? system, ... }:
    mapModules dir
      (hostPath: mkHost hostPath attrs);
}
