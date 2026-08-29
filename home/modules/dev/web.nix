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
      # Javascript Runtimes
      nodejs_24
      bun
      deno

      # Package Managers
      pnpm
      yarn

      # Linting
      # nodePackages.prettier

      # Typescript
      typescript
      typescript-language-server

      # Astrojs language server
      astro-language-server

      # Load Testing
      k6

      # Debugger for DAP (neovim, emacs)
      vscode-js-debug

      # API testing GUI
      bruno
    ];
  };
}
