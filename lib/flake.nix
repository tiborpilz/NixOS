# lib/flake.nix
{
  description = "Helper functions for NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; # Will be overridden
  };

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      importLib = path: import path { inherit lib; self = { }; };

      attrs = importLib ./attrs.nix;
      defaults = importLib ./defaults.nix;
      flakehelper = importLib ./flakehelper.nix;
      modules = importLib ./modules.nix;
      nixos = importLib ./nixos.nix;
      options = importLib ./options.nix;

      myLib = attrs // defaults // flakehelper // modules // nixos // options;
    in {
      # Library functions for NixOS and Darwin configurations
      lib = myLib // {
        modules = {
         inherit attrs defaults flakehelper modules nixos options;
        };
      };
    };
}
