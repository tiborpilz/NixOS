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
    # modules.shell.zsh.fpathDirs = "${pkgs.unstable.bun}/share/zsh/site-functions";
    modules.shell.zsh.fpathDirs = "${pkgs.unstable.bun}/share/zsh/site-functions";

    # Packages for web development, mostly for JavaScript
    home.packages = with pkgs.unstable; [
      # Javascript Runtimes
      nodejs_24
      bun
      deno

      # Package Managers
      nodePackages.pnpm
      nodePackages.yarn

      # Linting
      # nodePackages.prettier

      # Typescript
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.ts-node

      # Vue
      vue-language-server

      # Astrojs language server
      nodePackages."@astrojs/language-server"

      # Load Testing
      k6

      # Debugger for DAP (neovim, emacs)
      vscode-js-debug

      # API testing GUI
      bruno
    ];
  };
}
