{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    sops-nix,
    flake-utils,
    flake-utils-plus,
    ...
  } @ inputs:
    let
      inherit (builtins) removeAttrs;
      lib = nixpkgs.lib.extend
        (self: super: {
          my = import ./lib { inherit inputs; lib = self; pkgs = nixpkgs; };
          hm = home-manager.lib;
        });
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      pkgs = self.pkgs.x86_64-linux.nixpkgs;

    in flake-utils-plus.lib.mkFlake {
      inherit self inputs;
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

      channels.unstable.input = nixpkgs-unstable;

      outputsBuilder = channels: {
        # packages = { inherit (channels.unstable) hello; } // (import ./packages { pkgs = channels.unstable; });
        packages = lib.foldAttrs (item: acc: item) {} (lib.attrValues (mapModules ./packages (p: import p { pkgs = channels.unstable; })));
        overlays = (mapModules ./overlays import);

        apps.repl = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };
      };

      overlays.default = final: prev: {
        unstable = nixpkgs-unstable;
        my = self.packages;
      };

      inherit lib;
    };


      # mkPkgs = pkgs: extraOverlays: system: import pkgs {
      #   inherit system;
      #   config.allowUnfree = true;
      #   overlays = extraOverlays;
      # };
    # in {
    #   packages = eachSystem systems (system:
    #     let
    #       pkgs = mkPkgs nixpkgs [] system;
    #     in {
    #       default = pkgs.hello;
    #     });

    #   overlays = eachSystem systems(system: (mapModules ./overlays import));
    # };
}

      # mkPkgs = pkgs: extraOverlays: system: import pkgs {
      #   inherit system;
      #   config.allowUnfree = true;
      #   overlays = extraOverlays ++ (lib.attrValues self.overlays);
      # };

      # pkgs = mkPkgs nixpkgs [ self.overlays.default ];


      # overlays = eachSystem systems (system:
      #   (mapModules ./overlays import) // {
      #     default = final: prev: {
      #       unstable = mkPkgs nixpkgs-unstable;
      #       my = self.packages."${system}";
      #     };
      #   });

      # lib = nixpkgs.lib.extend
      #   (self: super: {
      #     my = import ./lib { inherit pkgs inputs; lib = self; };
      #     hm = home-manager.lib;
#         });

#     in {
#       inherit packages overlays;
#       lib = lib.my;

#       # ."${system}" = {
#       #   default = pkgs.hello;
#       # } // import ./packages/node/default.nix {
#       #   inherit pkgs system;
#       # };

#       # devShells."x86_64-linux".default =
#       #   import ./shell.nix { inherit pkgs; };

#       nixosModules =
#         { dotfiles = import ./.; } // mapModulesRec ./modules import;

#       nixosConfigurations =
#         mapHosts ./hosts {};

#       homeConfigurations.tibor = home-manager.lib.homeManagerConfiguration {
#         inherit pkgs;
#         inherit lib;

#         modules = [
#           ./home
#           {
#             _module.args.inputs = inputs;
#             home.username = "tibor";
#             home.homeDirectory = "/home/tibor";
#             modules.syncthing.service = true;
#           }
#         ];
#       };

#       homeConfigurations.tibor-darwin = home-manager.lib.homeManagerConfiguration {
#         pkgs = import inputs.nixpkgs-unstable {
#           system = "x86_64-darwin";
#           config.allowUnfree = true;
#           overlays = lib.attrValues {
#             default = final: prev: {
#               unstable = mkPkgs nixpkgs-unstable;
#               my = self.packages.x86_64-darwin;
#             };
#           };
#         };
#         inherit lib;

#         modules = [
#           ./home
#           {
#             _module.args.inputs = inputs;
#             home.username = "tibor.pilz";
#             home.homeDirectory = "/Users/tibor.pilz";
#           }
#         ];
#       };
#     };
# }
