{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    plasma-manager.url = "github:nix-community/plasma-manager";
    plasma-manager.inputs.nixpkgs.follows = "nixpkgs";
    plasma-manager.inputs.home-manager.follows = "home-manager";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    deno2nix.url = "github:SnO2WMaN/deno2nix";
    devshell.url = "github:numtide/devshell";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , sops-nix
    , flake-utils
    , flake-utils-plus
    , ...
    } @ inputs:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      lib = nixpkgs.lib.extend
        (self: super: {
          my = import ./lib { inherit inputs; lib = self; pkgs = nixpkgs; };
          hm = home-manager.lib;
        });
      inherit (lib.my) mapModules;

      pkgs = self.pkgs.x86_64-linux.nixpkgs;

      nixosHosts = mapModules ./hosts/nixos (hostPath: lib.my.mkHostAttrs hostPath {
        system = "x86_64-linux";
        modules = lib.my.mapModulesRec' (toString ./modules/nixos) import;
      });

      darwinHosts = mapModules ./hosts/darwin (hostPath: lib.my.mkHostAttrs hostPath {
        system = "aarch64-darwin";
        modules = lib.my.mapModulesRec' (toString ./modules/darwin) import;
      });

    in
    flake-utils-plus.lib.mkFlake rec {
      inherit lib self inputs supportedSystems;

      channels.nixpkgs-unstable.config = { allowUnfree = true; };
      channels.nixpkgs.config = { allowUnfree = true; };

      hostDefaults = {
        channelName = "nixpkgs";
        modules = [
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
        ] ++ lib.my.mapModulesRec' (toString ./modules/shared) import;
      };

      sharedOverlays = [
        (final: prev: {
          unstable = import nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };
          my = self.packages."${prev.system}";
        })
        inputs.deno2nix.overlays.default
        inputs.devshell.overlays.default
        inputs.emacs-overlay.overlays.default
      ];

      hosts = nixosHosts;

      outputsBuilder = channels: rec {
        inherit channels;

        packages = lib.foldAttrs (item: acc: item) { }
          (lib.attrValues (mapModules ./packages (p: import p {
            inherit lib inputs;
            pkgs = channels.nixpkgs;
          }))) // {
          testTandoor = pkgs.testers.runNixOSTest ./tests/tandoor.nix;
          # testPaperless = pkgs.testers.runNixOSTest ./tests/paperless.nix;
        };

        apps = (lib.mapAttrs' (name: value: { inherit name; value = lib.my.mkApp value; }) packages) // {
          default = apps.flakeRepl;
        };

        devShells = {
          default = import ./shell.nix { pkgs = channels.nixpkgs; };
        };

        formatter = pkgs.nixpkgs-fmt;


      };

      homeConfigurations = lib.my.mergeAttrs (lib.forEach supportedSystems (system:
        let
          isDarwin = (system == "x86_64-darwin" || system == "aarch64-darwin");
          user = if (isDarwin) then "tibor.pilz" else "tibor";
          homeDirectory = if (isDarwin) then "/Users/${user}" else "/home/${user}";
          pkgs = self.channels.${system}.nixpkgs;
          enableSyncthing = (system == "x86_64-linux");
          hosts = lib.attrNames self.nixosConfigurations;
          mkHostAliases = map (h: "${user}@${h}") hosts;
          aliases = mkHostAliases;
          homeConfiguration = home-manager.lib.homeManagerConfiguration {
            inherit lib pkgs;

            modules = [
              ./home
              inputs.nix-doom-emacs-unstraightened.hmModule
              {
                _module.args.inputs = inputs;
                home.username = user;
                home.homeDirectory = homeDirectory;
                modules.syncthing.service = enableSyncthing;
                nix.package = pkgs.nix;
              }
            ];
          };
          aliasConfigurations = lib.foldr (curr: prev: prev // { "${curr}" = homeConfiguration; }) { } aliases;
        in
        { "${user}" = homeConfiguration; } // aliasConfigurations
      ));

      nixosModules = lib.my.mapModulesRec (toString ./modules) import;
    };
}
