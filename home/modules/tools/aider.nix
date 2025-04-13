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
        if [ -z "$OPENAI_API_KEY" ]; then
          # TODO: make key(s) configurable in global config
          export OPENAI_API_KEY=$(pass bitwarden/openai-api-key)
        fi
        if [ -z "$ANTHROPIC_API_KEY" ]; then
          # TODO: make key(s) configurable in global config
          export ANTHROPIC_API_KEY=$(pass bitwarden/anthropic-api-key)
        fi
        ${pkgs.unstable.aider-chat}/bin/aider "$@"
      }
    '';
  };
}
