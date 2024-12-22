{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.dev.gleam;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.dev.gleam = {
    enable = mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      erlang_27
      unstable.gleam
      # Dependencies for glacier (incremental test runner)
      rebar3
      inotify-tools
    ];
  };
}
