{
  description = "NixOS configuration for a workstation and a homeserver";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, sops-nix, ... }:
    let
      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
      pkgs  = mkPkgs nixpkgs [ self.overlays.default ];
      pkgs' = mkPkgs nixpkgs-unstable [];

    in {
      lib = lib.my;

      overlays =
        (mapModules ./overlays import) // {
          default = final: prev: {
            unstable = pkgs';
            my = self.packages."${system}";
          };
        };

      packages."${system}" =
        (mapModules ./packages (p: pkgs.callPackage p {}))
        // { default = pkgs.hello; };

      nixosModules =
        { dotfiles = import ./.; } // mapModulesRec ./modules import;

      nixosConfigurations =
        mapHosts ./hosts {};

      devShells."${system}".default =
        import ./shell.nix { inherit pkgs; };

      # formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
      # nixosConfigurations = {
      #   workyMcNixStation = nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     modules = [
      #       (builtins.toPath "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
      #       (builtins.toPath "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
      #       ./hosts/workyMcNixStation
      #       home-manager.nixosModules.home-manager
      #       {
      #         home-manager.useGlobalPkgs = true;
      #         home-manager.useUserPackages = true;
      #         home-manager.users.tibor = import ./home-manager/home.nix;
      #       }
      #       sops-nix.nixosModules.sops
      #     ];
      #   };
      #   homeserver = nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     modules = [
      #       ./hosts/homeserver
      #       sops-nix.nixosModules.sops
      #     ];
      #   };
      #   edge = nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     modules = [
      #       ./hosts/edge
      #       sops-nix.nixosModules.sops
      #     ];
      #   };
      #   ideapad = nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     modules = [
      #       ./hosts/ideapad
      #       sops-nix.nixosModules.sops
      #     ];
      #   };
      #   testvm = nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     modules = [
      #       (builtins.toPath "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix")
      #       (builtins.toPath "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix")
      #       ./hosts/homeserver
      #       sops-nix.nixosModules.sops
      #     ];
      #   };
      # };

      homeConfigurations.tibor = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./home.nix
          { _module.args.inputs = inputs; }
        ];
      };
    };
}
