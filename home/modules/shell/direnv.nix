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
    programs.direnv = {
      enable = true;

      config = {
        global = {
          load_dotenv = false;
        };
      };

      nix-direnv.enable = true;
    };

    modules.shell.zsh.rcInit = ''
      export DIRENV_LOG_FORMAT=
      eval "$(direnv hook zsh)"
    '';
  };
}
