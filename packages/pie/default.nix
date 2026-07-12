{ inputs, pkgs, ... }:

# Pie — the dependently-typed language from *The Little Typer*. Not in nixpkgs,
# so we build it in-repo from the pinned upstream source (`pie-src` flake
# input). Exposed as `pkgs.my.pie` (and `self.packages.<system>.pie`, so
# `nix run .#pie` opens a REPL).
{
  pie = pkgs.callPackage ./pie.nix { pie-src = inputs.pie-src; };
}
