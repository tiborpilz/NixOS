{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.tools.pi;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.tools.pi = {
    enable = mylib.mkBoolOpt false;

    apiKeyPassPath = mylib.mkOpt' lib.types.str "bitwarden/OPENROUTER_API_KEY" ''
      `pass` entry that holds the OpenRouter API key.
    '';
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.unstable.pi-coding-agent
    ];

    modules.shell.zsh.rcInit = ''
      function pi() {
        # pass entries may carry metadata lines after the secret; use only the first line
        if [ -z "$OPENROUTER_API_KEY" ]; then
          export OPENROUTER_API_KEY=$(pass ${cfg.apiKeyPassPath} | head -n1)
        fi
        command pi "$@"
      }
    '';
  };
}
