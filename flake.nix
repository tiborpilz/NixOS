{
  description = "NixOS and Home-Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-26.05";
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

    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";

    quadlet-nix.url = "github:SEIAROTg/quadlet-nix";

    determinate-nix.url = "github:DeterminateSystems/nix-src";
    determinate-nix.inputs.nixpkgs.follows = "nixpkgs";

    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3.16.0";
    determinate.inputs.nix.follows = "determinate-nix";
    determinate.inputs.nixpkgs.follows = "nixpkgs";

    claude-code.url = "github:sadjow/claude-code-nix";
    claude-code.inputs.nixpkgs.follows = "nixpkgs";

    pie-src = {
      url = "github:the-little-typer/pie/2c89553a693ac6688b16d722f416914f2e9aa4c3";
      flake = false;
    };

    nixos-wsl.url = "github:nix-community/nixos-wsl/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , sops-nix
    , flake-utils-plus
    , quadlet-nix
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

      nixosHosts = mapModules ./hosts/nixos (hostPath: lib.my.mkHostAttrs hostPath {
        system = "x86_64-linux";
        modules = lib.my.mapModulesRec' (toString ./modules/nixos) import
                  ++ [quadlet-nix.nixosModules.quadlet];
      });

      # darwinHosts = mapModules ./hosts/darwin (hostPath: lib.my.mkHostAttrs hostPath {
      #   system = "aarch64-darwin";
      #   modules = lib.my.mapModulesRec' (toString ./modules/darwin) import;
      # });

    in
    flake-utils-plus.lib.mkFlake
      rec {
        inherit lib self inputs supportedSystems;

        # Necessary for logseq
        channels.nixpkgs-unstable.config = {
          allowUnfree = true;
          permittedInsecurePackages = [ "electron-39.8.10" ];
        };
        channels.nixpkgs.config = {
          allowUnfree = true;
          permittedInsecurePackages = [ "electron-39.8.10" ];
        };

        hostDefaults = {
          channelName = "nixpkgs";
          modules = [
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            determinate.nixosModules.default
            quadlet-nix.nixosModules.quadlet
            inputs.disko.nixosModules.disko
          ] ++ lib.my.mapModulesRec' (toString ./modules/shared) import;
        };

        sharedOverlays = [
          (final: prev: {
            unstable = import nixpkgs-unstable {
              system = prev.stdenv.hostPlatform.system;
              config.allowUnfree = true;
              overlays = [ inputs.emacs-overlay.overlays.default ];
            };
            my = self.packages."${prev.stdenv.hostPlatform.system}";
            copilot-language-server-fhs = final.copilot-language-server;
          })
          inputs.devshell.overlays.default
          inputs.nix-doom-emacs-unstraightened.overlays.default
          inputs.claude-code.overlays.default
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
            testTandoorUpgrade = channels.nixpkgs.testers.runNixOSTest (import ./tests/upgrade/tandoor.nix {
              inherit inputs lib; pkgs = channels.nixpkgs;
            });
            testPaperlessUpgrade = channels.nixpkgs.testers.runNixOSTest (import ./tests/upgrade/paperless.nix {
              inherit inputs lib; pkgs = channels.nixpkgs;
            });
          };

          apps = (lib.mapAttrs' (name: value: { inherit name; value = lib.my.mkApp value; }) packages) // {
            default = apps.flakeRepl;
          };

          devShells = import ./shell.nix { pkgs = channels.nixpkgs; };

          formatter = channels.nixpkgs.nixpkgs-fmt;

        };

        homeConfigurations = lib.my.mergeAttrs (lib.forEach supportedSystems (system:
          let
            isDarwin = (system == "x86_64-darwin" || system == "aarch64-darwin");
            user = if (isDarwin) then "tiborpilz" else "tibor";
            homeDirectory = if (isDarwin) then "/Users/${user}" else "/home/${user}";
            pkgs = self.channels.${system}.nixpkgs;
            enableSyncthing = (system == "x86_64-linux");
            hosts = lib.attrNames self.nixosConfigurations;
            mkHostAliases = map (h: "${user}@${h}") hosts;
            aliases = mkHostAliases;
            homeConfiguration = home-manager.lib.homeManagerConfiguration {
              inherit lib pkgs;

              modules = [
                inputs.nix-doom-emacs-unstraightened.homeModule
                ./home
                {
                  _module.args.inputs = inputs;
                  home.username = user;
                  home.homeDirectory = homeDirectory;
                  modules.syncthing.service = enableSyncthing;
                  # snapcast client for Music Assistant's snapserver on klaus
                  modules.snapclient.enable = isDarwin;
                }
              ];
            };
            aliasConfigurations = lib.foldr (curr: prev: prev // { "${curr}" = homeConfiguration; }) { } aliases;
          in
          { "${user}" = homeConfiguration; } // aliasConfigurations
        ));

        nixosModules = lib.my.mapModulesRec (toString ./modules) import;
      } // {
      # SSH transport (reaching ssh.tiborpilz.xyz via the tunnel) lives in
      # ~/.ssh/config, not here. See docs/deploy-rs.md.
      deploy.nodes.klaus = {
        hostname = "ssh.tiborpilz.xyz";
        sshUser = "root";
        magicRollback = true;
        # Slack for the confirm reconnect if activation restarts cloudflared.
        confirmTimeout = 60;
        profiles.system = {
          user = "root";
          # Build on the target (mirrors the old --build-host = klaus).
          remoteBuild = true;
          path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.klaus;
        };
      };

      # kind = "live" builds a self-contained live system (see lib/live.nix);
      # everything else is an installer for one of the hosts above.
      isos = mapModules ./isos (path:
        let spec = import path; in
        if (spec.kind or "installer") == "live"
        then lib.my.mkLiveIso spec
        else lib.my.mkIso spec);

      checks = {
        x86_64-linux = {
          home-tibor = self.homeConfigurations.tibor.activationPackage;
          edge = self.nixosConfigurations.edge.config.system.build.toplevel;
          klaus = self.nixosConfigurations.klaus.config.system.build.toplevel;
          thinkyMcThinkpad = self.nixosConfigurations.thinkyMcThinkpad.config.system.build.toplevel;
          couchthink = self.nixosConfigurations.couchthink.config.system.build.toplevel;
          emacs = self.packages.x86_64-linux.emacsWrapped;
          doom-emacs = self.packages.x86_64-linux.doom-emacs;
          doom-emacs-standalone = self.packages.x86_64-linux.doom-emacs-standalone;
          testTandoorUpgrade = self.packages.x86_64-linux.testTandoorUpgrade;
          testPaperlessUpgrade = self.packages.x86_64-linux.testPaperlessUpgrade;
        } // inputs.deploy-rs.lib.x86_64-linux.deployChecks self.deploy;
        aarch64-darwin = {
          home-tiborpilz = self.homeConfigurations.tiborpilz.activationPackage;
          emacs = self.packages.aarch64-darwin.emacsWrapped;
          doom-emacs = self.packages.aarch64-darwin.doom-emacs;
          doom-emacs-standalone = self.packages.aarch64-darwin.doom-emacs-standalone;
        };
      };
    };
}
