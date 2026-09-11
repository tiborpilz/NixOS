{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.devenv;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.devenv = with types; {
    enable = mylib.mkBoolOpt false;

    useHook = mylib.mkOpt' bool false ''
      Activate devenv projects from a zsh hook. Recommended approach, but will change the behavior
      of the shell prompt. If false, use either `devenv shell` or `.envrc`.
    '';

    # `pkgs.my` is backed by this repository's package outputs, where devenv
    # comes directly from the upstream devenv flake.
    package = mylib.mkOpt package pkgs.my.devenv;

    shell = mylib.mkOpt' (enum [ "bash" "zsh" "fish" "nu" ]) "zsh" ''
      Interactive shell that `devenv shell` drops into.
    '';
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.sessionVariables.DEVENV_SHELL_TYPE = cfg.shell;

    xdg.configFile."devenv/config.yaml".source = ../../config/devenv/config.yaml;

    modules.shell.zsh.rcInit = mkIf cfg.useHook ''
      eval "$(devenv hook zsh -- --no-tui --quiet)"
    '';
  };
}
