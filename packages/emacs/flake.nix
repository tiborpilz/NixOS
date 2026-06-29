{
  description = "Emacs packages and modules";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    nix-doom-emacs-unstraightened.inputs.nixpkgs.follows = "nixpkgs";

    claude-code.url = "github:sadjow/claude-code-nix";
    claude-code.inputs.nixpkgs.follows = "nixpkgs";
    claude-code.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    { nixpkgs
    , nixpkgs-unstable
    , flake-utils
    , emacs-overlay
    , nix-doom-emacs-unstraightened
    , claude-code
    , ...
    }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];

      mkPkgs = system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            unstable = import nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
              overlays = [ emacs-overlay.overlays.default ];
            };
            ghostscript = nixpkgs-unstable.legacyPackages.${prev.system}.ghostscript;
            copilot-language-server-fhs = final.copilot-language-server;
          })
          nix-doom-emacs-unstraightened.overlays.default
          claude-code.overlays.default
          emacs-overlay.overlays.default
        ];
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system: {
      packages = import ./default.nix {
        lib = nixpkgs.lib;
        pkgs = mkPkgs system;
      };
    }) // {
      homeModules.default = nix-doom-emacs-unstraightened.homeModule;
    };
}
