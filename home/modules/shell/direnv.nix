{ inputs, config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.direnv;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.direnv = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.direnv ];
    modules.shell.zsh.rcInit = ''
      export DIRENV_LOG_FORMAT=
      eval "$(direnv hook zsh)"
    '';

    xdg.configFile."direnv/direnv.toml".text = ''
      [global]
      load_dotenv = false
    '';
  };
}
