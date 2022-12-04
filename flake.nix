{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";


    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    digga.url = "github:divnix/digga";

    doom-emacs-config.url = "github:tiborpilz/doom-emacs-config/feat/remove-recipe-packages";
    doom-emacs-config.flake = false;

    copilot-el.url = "github:zerolfx/copilot.el";
    copilot-el.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    sops-nix,
    flake-utils,
    flake-utils-plus,
    digga,
    ...
  } @ inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      lib = nixpkgs.lib.extend
        (self: super: {
          my = import ./lib { inherit inputs; lib = self; pkgs = nixpkgs; };
          hm = home-manager.lib;
        });
      inherit (lib.my) mapModules;

      pkgs = self.pkgs.x86_64-linux.nixpkgs;

    in flake-utils-plus.lib.mkFlake rec {
      inherit self inputs supportedSystems;

      channels.nixpkgs-unstable.config = { allowUnfree = true; };
      channels.nixpkgs.config = { allowUnfree = true; };

      hostDefaults = {
        channelName = "nixpkgs";
        modules = [
          digga.nixosModules.bootstrapIso
          digga.nixosModules.nixConfig
          home-manager.nixosModules.home-manager
        ] ++ lib.my.mapModulesRec' (toString ./modules) import;
      };

      sharedOverlays = [
        (final: prev: {
          unstable = import nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };
          my = self.packages."${prev.system}";
        })
      ];

      hosts = mapModules ./hosts (hostPath: lib.my.mkHostAttrs hostPath { });

      outputsBuilder = channels: rec {
        packages = lib.foldAttrs (item: acc: item) {} (lib.attrValues (mapModules ./packages (p: import p { inherit inputs; pkgs = channels.nixpkgs; })));

        apps.default = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };

        devShells = {
          default = import ./shell.nix { pkgs = channels.nixpkgs; };
        };

      };

      # homeConfigurations = digga.lib.mkHomeConfigurations self.nixosConfigurations;

      homeConfigurations.tibor = self.nixosConfigurations.edge.config.home-manager.users.tibor.home; # home-manager.lib.homeManagerConfiguration {
      #   inherit lib;
      #   pkgs = self.pkgs;

      #   modules = [ self.nixosConfigurations.edge.config.home-manager.users.tibor.home ];
      # };

      nixosModules = lib.my.mapModulesRec (toString ./modules) import;

      # homeConfigurations = (lib.my.mergeAttrs (lib.forEach supportedSystems (system: {
      #   "tibor-${system}" = home-manager.lib.homeManagerConfiguration {
      #     inherit lib;
      #     pkgs = self.pkgs;

      #     modules = [
      #       ./home
      #       {
      #         _module.args.inputs = inputs;
      #         home.username = "tibor";
      #         home.homeDirectory = "/home/tibor";
      #         modules.syncthing.service = true;
      #       }
      #     ];
      #   };
      # })));

      inherit lib;
    };
}

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
