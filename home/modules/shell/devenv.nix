{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.devenv;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.devenv = with types; {
    enable = mylib.mkBoolOpt false;
    useHook = mylib.mkOpt' bool false "Whether to enable 'devenv hook' in zsh";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.devenv ];
    home.sessionVariables.DEVENV_SHELL_TYPE = "zsh";

    xdg.configFile."devenv/config.yaml".source = ../../config/devenv/config.yaml;

    modules.shell.zsh.rcInit = mkIf cfg.useHook ''
      eval "$(devenv hook zsh -- --no-tui --quiet)"
    '';
  };
}
