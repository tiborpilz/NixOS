{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.jsonnet;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.jsonnet = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      jsonnet
      jsonnet-bundler
      jsonnet-language-server
    ];
  };
}
