{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

  };
  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, sops-nix, ... }:
    let
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

      pkgs  = mkPkgs nixpkgs [ self.overlays.default ];
      pkgs' = mkPkgs nixpkgs-unstable [];

      lib = nixpkgs.lib.extend
        (self: super: {
          my = import ./lib { inherit pkgs inputs; lib = self; };
          hm = home-manager.lib;
        });

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

      homeConfigurations.tibor = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        inherit lib;

        modules = [
          ./home
          {
            _module.args.inputs = inputs;
            home.username = "tibor";
            home.homeDirectory = "/home/tibor";

          }
        ];
      };

      homeConfigurations.tibor-darwin = home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs-unstable {
          system = "x86_64-darwin";
          config.allowUnfree = true;
          overlays = lib.attrValues {
            default = final: prev: {
              unstable = pkgs';
            };
          };
        };
        inherit lib;

        modules = [
          ./home
          {
            _module.args.inputs = inputs;
            home.username = "tibor.pilz";
            home.homeDirectory = "/Users/tibor.pilz";
          }
        ];
      };
    };
}
