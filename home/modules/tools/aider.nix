# Aider module
# Aider is an AI assistant in the terminal for editing codebases.
# (https://github.com/paul-gauthier/aider)
# This module installs the necessary package and sets the OpenAI API key via
# the environment variable.
# It expects the password to be available in `pass` via a given key.
{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.tools.aider;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.tools.aider = {
    enable = mylib.mkBoolOpt false;
  };

  config = lib.mkIf cfg.enable {
    home.packages =  [
      pkgs.unstable.aider-chat
    ];

    modules.shell.zsh.rcInit = ''
      function aider() {
        # TODO: make key(s) and API base configurable in global config
        if [ -z "$OPENAI_API_KEY" ]; then
          export OPENAI_API_KEY=$(pass bitwarden/IU_OPENAI_API_KEY)
        fi

        if [ -z "$OPENAI_API_BASE" ]; then
          export OPENAI_API_BASE=$(pass bitwarden/IU_OPENAI_API_BASE)
        fi
        ${pkgs.unstable.aider-chat}/bin/aider --no-auto-commits "$@"
      }
    '';
  };
}
