{
  description = "NixOS configuration for a workstation and a homeserver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager.url = "github:nix-community/home-manager";
    sops-nix.url = "github:Mic92/sops-nix";
  };
  outputs = { home-manager, nixpkgs, ... }: {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
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
          ./machines/homeserver/configuration.nix
        ];
      };
      edge = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/edge/configuration.nix
        ];
      };
      ideapad = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/ideapad/configuration.nix
        ];
      };
      testvm = nixpkgs.lib.nixosSystem {
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
