{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.scripts;
  mylib = import ../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.scripts = {
    enable = mylib.mkBoolOpt false;
  };

  # TODO: Package scripts correctly
  config = mkIf cfg.enable {
    home.file."bin" = {
      source = ../scripts;
      recursive = true;
    };

    # Add $HOME/bin to $PATH
    modules.shell.zsh.envInit = ''
      export PATH="$HOME/bin:$PATH"
    '';

    # Add $HOME/bin/completions to $fpath
    modules.shell.zsh.rcInit = ''
      fpath+=($HOME/bin/completions)
    '';
  };
}
