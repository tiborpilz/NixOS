{ inputs, config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.direnv;
    mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.shell.direnv = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.direnv ];
    modules.shell.zsh.rcInit = ''eval "$(direnv hook zsh)"'';
  };
}
