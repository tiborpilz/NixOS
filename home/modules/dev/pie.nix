{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.pie;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.pie = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    # Pie REPL (`pie`) plus pie-aware `racket`/`raco` wrappers.
    home.packages = [ pkgs.my.pie ];
  };
}
