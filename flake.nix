{
  description = "NixOS configuration for a workstation and a homeserver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager.url = "github:nix-community/home-manager";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, home-manager, nixpkgs, sops-nix, ... }: {
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
          sops-nix.nixosModules.sops
        ];
      };
      homeserver = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/homeserver/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
      edge = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/edge/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
      ideapad = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/ideapad/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
      testvm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (builtins.toPath "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
          (builtins.toPath "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
          ./machines/homeserver/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
    };
  };
}
