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

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , sops-nix
    , flake-utils
    , flake-utils-plus
    , digga
    , ...
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

    in
    flake-utils-plus.lib.mkFlake rec {
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
        packages = lib.foldAttrs (item: acc: item) { } (lib.attrValues (mapModules ./packages (p: import p { inherit inputs; pkgs = channels.nixpkgs; })));

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

        formatter = pkgs.nixpkgs-fmt;

        pkgs = channels.nixpkgs;
      };

      homeConfigurations = lib.my.mergeAttrs (lib.forEach supportedSystems (system:
        let
          user = if (system == "x86_64-darwin") then "tibor.pilz" else "tibor";
          homeDirectory = if (system == "x86_64-darwin") then "/home/${user}" else "/Users/${user}";
          pkgs = self.pkgs."${system}";
          enableSyncthing = (system == "x86_64-linux");
        in
        {
          "${user}" = home-manager.lib.homeManagerConfiguration {
            inherit pkgs lib;

            modules = [
              ./home
              inputs.nix-doom-emacs.hmModule
              {
                _module.args.inputs = inputs;
                home.username = user;
                home.homeDirectory = homeDirectory;
                modules.syncthing.service = enableSyncthing;
              }
            ];
          };
        }
      ));

      nixosModules = lib.my.mapModulesRec (toString ./modules) import;

      inherit lib;
    };
}
