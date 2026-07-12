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
    # Pie REPL (`pie`) plus pie-aware `racket`/`raco` wrappers, built in-repo
    # from packages/pie (exposed as pkgs.my.pie). Doom's racket-mode and the
    # Neovim ftplugin both resolve `racket` via PATH, so this one package serves
    # both editors; editor integration itself lives in this repo.
    home.packages = [ pkgs.my.pie ];
  };
}
