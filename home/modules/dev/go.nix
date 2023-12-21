{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.module.go;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.module.go = {
    enable = mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      go
      gopls
    ];
  };

  home.sessionVariables = rec {
    GOPATH = "${config.home}/.local/go";
    GOBIN = "${config.home.sessionVariables.GOPATH}/bin";
    PATH = "${config.home.sessionVariables.GOBIN}:${config.home.sessionVariables.PATH}";
  };
}
