{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.nix;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  nhPackage = pkgs.unstable.nh;
in
{
  options.modules.dev.nix = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    modules.shell.zsh.fpathDirs = "${nhPackage}/share/zsh/site-functions";

    home.packages = with pkgs; [
      nhPackage

      # alternative lsp server for nix
      nil
    ];
  };
}
