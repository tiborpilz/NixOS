{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs-24-05.url = "nixpkgs/nixos-24.05";
    nixpkgs.url = "nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    plasma-manager.url = "github:nix-community/plasma-manager";
    plasma-manager.inputs.nixpkgs.follows = "nixpkgs";
    plasma-manager.inputs.home-manager.follows = "home-manager";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    nix-doom-emacs-unstraightened.inputs.nixpkgs.follows = "nixpkgs";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    devshell.url = "github:numtide/devshell";

    deploy-rs.url = "github:serokell/deploy-rs";

    quadlet-nix.url = "github:SEIAROTg/quadlet-nix";

    determinate-nix.url = "github:DeterminateSystems/nix-src";
    determinate-nix.inputs.nixpkgs.follows = "nixpkgs";

    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3.16.0";
    determinate.inputs.nix.follows = "determinate-nix";

    radicle-explorer.url = "git+https://iris.radicle.xyz/z4V1sjrXqjvFdnCUbxPFqd5p4DtH5.git";

    claude-code.url = "github:sadjow/claude-code-nix";

    rio.url = "github:raphamorim/rio/main";
  };
      # 'aradicle-explorer.inputs.nixpkgs.follows = "nixpkgs-unstable";

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , sops-nix
    , deploy-rs
    , quadlet-nix
    , radicle-explorer
    , determinate
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
      emacs = (import ./packages/emacs/flake.nix).outputs {
        inherit nixpkgs nixpkgs-unstable;
        inherit (inputs) flake-utils emacs-overlay nix-doom-emacs-unstraightened claude-code;
      };

      mkUnstable = system: import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };

      sharedOverlays = [
        (final: prev: {
          unstable = mkUnstable prev.system;
          my = self.packages.${prev.system};
        })
        inputs.devshell.overlays.default
        inputs.claude-code.overlays.default
      ];

      channels = lib.genAttrs supportedSystems (system: {
        nixpkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = sharedOverlays;
        };
        nixpkgs-unstable = mkUnstable system;
      });

      sharedHostModules = [
        home-manager.nixosModules.home-manager
        sops-nix.nixosModules.sops
        radicle-explorer.nixosModules.radicle-explorer
        determinate.nixosModules.default
        quadlet-nix.nixosModules.quadlet
        inputs.disko.nixosModules.disko
      ] ++ lib.my.mapModulesRec' (toString ./modules/shared) import;

      nixosHosts = mapModules ./hosts/nixos (hostPath: lib.my.mkHostAttrs hostPath {
        system = "x86_64-linux";
        modules = lib.my.mapModulesRec' (toString ./modules/nixos) import
                  ++ [ quadlet-nix.nixosModules.quadlet ];
      });

      # darwinHosts = mapModules ./hosts/darwin (hostPath: lib.my.mkHostAttrs hostPath {
      #   system = "aarch64-darwin";
      #   modules = lib.my.mapModulesRec' (toString ./modules/darwin) import;
      # });

      packageSets = system:
        lib.attrValues (lib.filterAttrs (name: _: name != "emacs")
          (mapModules ./packages (p: import p {
            inherit lib inputs;
            pkgs = channels.${system}.nixpkgs;
          })));

    in
    rec {
      inherit lib inputs supportedSystems channels;
      pkgs = channels;

      nixosConfigurations = lib.mapAttrs (_: host:
        host.builder {
          inherit (host) system specialArgs;
          modules = sharedHostModules ++ host.modules;
        }
      ) nixosHosts;

      packages = lib.genAttrs supportedSystems (system:
        lib.foldAttrs (item: acc: item) { }
          (packageSets system ++ [ emacs.packages.${system} ]) // {
          testTandoorUpgrade = channels.${system}.nixpkgs.testers.runNixOSTest (import ./tests/upgrade/tandoor.nix {
            inherit inputs lib;
            pkgs = channels.${system}.nixpkgs;
          });
          testPaperlessUpgrade = channels.${system}.nixpkgs.testers.runNixOSTest (import ./tests/upgrade/paperless.nix {
            inherit inputs lib;
            pkgs = channels.${system}.nixpkgs;
          });
        });

      apps = lib.genAttrs supportedSystems (system:
        (lib.mapAttrs' (name: value: { inherit name; value = lib.my.mkApp value; }) packages.${system}) // {
          default = apps.${system}.flakeRepl;
        });

      devShells = lib.genAttrs supportedSystems (system:
        import ./shell.nix { pkgs = channels.${system}.nixpkgs; });

      formatter = lib.genAttrs supportedSystems (system:
        channels.${system}.nixpkgs.nixpkgs-fmt);

      homeConfigurations = lib.my.mergeAttrs (lib.forEach supportedSystems (system:
          let
            isDarwin = (system == "x86_64-darwin" || system == "aarch64-darwin");
            user = if (isDarwin) then "tiborpilz" else "tibor";
            homeDirectory = if isDarwin then "/Users/${user}" else "/home/${user}";
            pkgs = channels.${system}.nixpkgs;
            enableSyncthing = system == "x86_64-linux";
            hosts = lib.attrNames self.nixosConfigurations;
            mkHostAliases = map (h: "${user}@${h}") hosts;
            aliases = mkHostAliases;
            homeConfiguration = home-manager.lib.homeManagerConfiguration {
              inherit lib pkgs;

              modules = [
                emacs.homeModules.default
                ./home
                {
                  _module.args.inputs = inputs;
                  home.username = user;
                  home.homeDirectory = homeDirectory;
                  modules.syncthing.service = enableSyncthing;
                }
              ];
            };
            aliasConfigurations = lib.foldr (curr: prev: prev // { "${curr}" = homeConfiguration; }) { } aliases;
          in
          { "${user}" = homeConfiguration; } // aliasConfigurations
        ));

      nixosModules = lib.my.mapModulesRec (toString ./modules) import;

      deploy.nodes.klaus = {
        hostname = "klaus";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.klaus;
          remoteBuild = true;
        };
      };

      checks = {
        x86_64-linux = {
          home-tibor = self.homeConfigurations.tibor.activationPackage;
          edge = self.nixosConfigurations.edge.config.system.build.toplevel;
          klaus = self.nixosConfigurations.klaus.config.system.build.toplevel;
          thinkyMcThinkpad = self.nixosConfigurations.thinkyMcThinkpad.config.system.build.toplevel;
          doom-emacs = self.packages.x86_64-linux.doom-emacs;
          testTandoorUpgrade = self.packages.x86_64-linux.testTandoorUpgrade;
          testPaperlessUpgrade = self.packages.x86_64-linux.testPaperlessUpgrade;
        };
        aarch64-darwin = {
          home-tiborpilz = self.homeConfigurations.tiborpilz.activationPackage;
          doom-emacs = self.packages.aarch64-darwin.doom-emacs;
        };
      };
    };
}
