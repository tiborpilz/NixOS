# TODO: per-package flake.nix
{
  description = "Custom packages for my NixOS setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    # Emacs-specific inputs
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, emacs-overlay, emacs-lsp-booster, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];

      # Helper function to generate per-system attributes
      forAllSystems = func: nixpkgs.lib.genAttrs supportedSystems func;

      # Configure nixpkgs for each system
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay.overlays.default
          emacs-lsp-booster.overlays.default
          self.overlays.default
        ];
        config = { allowUnfree = true; };
      };

      # Create overlay for your packages
      myOverlay = final: prev: {
        my = {
          # Emacs packages
          emacs-wrapped = final.callPackage ./emacs/emacs-wrapped.nix {};

          # Scripts
          scripts = {
            fooScript = final.callPackage ./scripts/foo.nix {};
            barScript = final.callPackage ./scripts/bar.nix {};
          };
          
          # Utils
          utils = {
            flakeRepl = final.callPackage ./utils/flake-repl.nix {};
          };
        };
      };

      # Create app from a package
      mkApp = pkg: {
        type = "app";
        program = "${pkg}/bin/${pkg.meta.mainProgram or (builtins.baseNameOf pkg.name)}";
      };

    in {
      # Overlay to add these packages to nixpkgs
      overlays.default = myOverlay;

      # Export packages for each system
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in {
          # Explicitly export packages by name
          inherit (pkgs.my) emacs-wrapped;
          inherit (pkgs.my.scripts) fooScript barScript;
          inherit (pkgs.my.utils) flakeRepl;

          # Set default package
          default = pkgs.my.utils.flakeRepl;
        }
      );

      # Export apps for each system
      apps = forAllSystems (system:
        let
          pkgs = self.packages.${system};
        in {
          flakeRepl = mkApp pkgs.flakeRepl;
          # Add other apps as needed
          default = mkApp pkgs.flakeRepl;
        }
      );
    };
}
