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
    # modules.shell.zsh.fpathDirs = "${nhPackage}/share/zsh/site-functions";
    programs.nh.enable = true;
    programs.nh.package = nhPackage;

    home.packages = with pkgs; [
      # alternative lsp server for nix
      nil
    ];
  };
}
