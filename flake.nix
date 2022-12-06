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

    deno2nix.url = "github:SnO2WMaN/deno2nix";
    devshell.url = "github:numtide/devshell";

    deploy-rs.url = "github:serokell/deploy-rs";

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
    , deploy-rs
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

      nixosHosts = mapModules ./hosts/nixos (hostPath: lib.my.mkHostAttrs hostPath { system = "x86_64-linux"; });
      darwinHosts = mapModules ./hosts/darwin (hostPath: lib.my.mkHostAttrs hostPath { system = "x86_64-darwin"; });
    in
    flake-utils-plus.lib.mkFlake rec {
      inherit lib self inputs supportedSystems;

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
        inputs.deno2nix.overlay
        inputs.devshell.overlay
      ];

      hosts = nixosHosts // darwinHosts;

      outputsBuilder = channels: rec {
        inherit channels;

        packages = lib.foldAttrs (item: acc: item) { }
          (lib.attrValues (mapModules ./packages (p: import p {
            inherit lib inputs;
            pkgs = channels.nixpkgs;
          })));

        apps = (lib.mapAttrs' (name: value: { inherit name; value = lib.my.mkApp value; }) packages) // { default = apps.flakeRepl; };

        devShells = {
          default = import ./shell.nix { pkgs = channels.nixpkgs; };
        };

        formatter = pkgs.nixpkgs-fmt;
      };

      homeConfigurations = lib.my.mergeAttrs (lib.forEach supportedSystems (system:
        let
          user = if (system == "x86_64-darwin") then "tibor.pilz" else "tibor";
          homeDirectory = if (system == "x86_64-darwin") then "/Users/${user}" else "/home/${user}";
          pkgs = self.channels."${system}".nixpkgs;
          enableSyncthing = (system == "x86_64-linux");
          homeConfiguration = home-manager.lib.homeManagerConfiguration {
            inherit lib pkgs;

            modules = [
              ./home
              inputs.nix-doom-emacs.hmModule
              {
                _module.args.inputs = inputs;
                home.username = user;
                home.homeDirectory = homeDirectory;
                modules.syncthing.service = enableSyncthing;
                nix.package = pkgs.nix;
              }
            ];
          };
        in { "${user}" = homeConfiguration; }
           // { "${lib.replaceStrings ["."] [""] user}" = homeConfiguration; }
      )) // (lib.my.mkHomeAliases "tibor" self.nixosConfigurations self.homeConfigurations);

      nixosModules = lib.my.mapModulesRec (toString ./modules) import;
    };
}
