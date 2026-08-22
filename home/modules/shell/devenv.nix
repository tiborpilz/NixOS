{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.devenv;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # Get `devenv hook zsh` output once instead of during every .zshrc invocation
  hookZsh = pkgs.runCommand "devenv-hook-zsh" { } ''
    ${cfg.package}/bin/devenv --offline -q hook zsh > $out
  '';

  # Custom check whether we're in a devenv directory (or a subdirectory of one) to avoid calling `devenv hook-should-activate` unnecessarily.
  hookGuard = ''
    if (( $+functions[_devenv_hook] )); then
      functions[_devenv_hook_inner]=$functions[_devenv_hook]

      _devenv_hook() {
        local prev=$?
        if [[ -z ''${_DEVENV_HOOK_DIR:-} && $_DEVENV_HOOK_PWD != $PWD ]]; then
          local d=$PWD
          until [[ -f $d/devenv.nix || -f $d/devenv.yaml ]]; do
            if [[ $d == / || -z $d ]]; then
              _DEVENV_HOOK_PWD=$PWD
              _DEVENV_HOOK_UNTRUSTED=""
              return $prev
            fi
            d=''${d:h}
          done
        fi
        (exit $prev)
        _devenv_hook_inner
      }
    fi
  '';
in
{
  options.modules.shell.devenv = with types; {
    enable = mylib.mkBoolOpt false;

    package = mylib.mkOpt package pkgs.unstable.devenv;

    shell = mylib.mkOpt' (enum [ "bash" "zsh" "fish" "nu" ]) "zsh" ''
      Interactive shell that `devenv shell` drops into.
    '';
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.sessionVariables.DEVENV_SHELL_TYPE = cfg.shell;

    modules.shell.zsh.rcInit = ''
      source ${hookZsh}
      ${hookGuard}
    '';
  };
}
