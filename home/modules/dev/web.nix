{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.dev.web;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.dev.web = {
    enable = mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    # Packages for web development, mostly for JavaScript
    home.packages = with pkgs; [
      unstable.bun
      nodePackages.pnpm
      nodePackages.yarn
      nodePackages.prettier
      nodePackages."@vue/cli"
    ];
  };
}
