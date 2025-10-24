{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.lua;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.lua = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ lua5_1 luarocks];
  };
}
