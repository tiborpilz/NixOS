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
    programs.bun = {
      enable = true;
      # package = pkgs.unstable.bun;
    };

    modules.shell.zsh.fpathDirs = "{pkgs.unstable.bun}/share/zsh/site-functions";

    # Packages for web development, mostly for JavaScript
    home.packages = with pkgs; [
      nodePackages.pnpm
      nodePackages.yarn
      nodePackages.prettier
      nodePackages."@vue/cli"

      # typescript & typescript language server
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.ts-node

      # Astrojs language server
      nodePackages."@astrojs/language-server"

      # Load Testing
      k6
    ];
  };
}
