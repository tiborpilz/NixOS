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

    modules.shell.zsh.rcInit = ''
      fpath=(${pkgs.unstable.bun}/share/zsh/site-functions $fpath)
    '';

    # Packages for web development, mostly for JavaScript
    home.packages = with pkgs; [
      nodePackages.pnpm
      nodePackages.yarn
      nodePackages.prettier
      nodePackages."@vue/cli"

      # Load Testing
      k6
    ];
  };
}
