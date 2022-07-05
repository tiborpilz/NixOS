{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager.url = "github:nix-community/home-manager";
  };
  outputs = { home-manager, nixpkgs, ... }: {
    nixosConfigurations = {
      workyMcNixStation = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (builtins.toPath "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
          (builtins.toPath "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
          ./machines/workyMcNixStation/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.tibor = import ./home-manager/home.nix;
          }
        ];
      };
      homeserver = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (builtins.toPath "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
          (builtins.toPath "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
          ./machines/homeserver/configuration.nix
        ];
      };
    };
  };
}
