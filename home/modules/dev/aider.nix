# Aider module
# Aider is an AI assistant in the terminal for editing codebases.
# (https://github.com/paul-gauthier/aider)
# This module installs the necessary package and sets the OpenAI API key via
# the environment variable.
# It expects the password to be available in `pass` via a given key.
{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.aider;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.aider = {
    enable = mylib.mkBoolOpt false;
    passApiKey = lib.mkOption {
      type = lib.types.str;
      default = "bitwarden/openai-api-key";
      description = "The key to use to retrieve the OpenAI API key from `pass`.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =  [
      pkgs.unstable.aider-chat
    ];

    programs.zsh = {
      enable = true;
      aliases = {
        aider = "OPENAI_API_KEY=$(pass ${cfg.passApiKey}) aider";
      };
    };
  };
}
