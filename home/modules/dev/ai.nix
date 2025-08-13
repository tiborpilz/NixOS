{inputs, config, lib, pkgs, ... }:

let
  cfg = config.modules.dev.ai;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.dev.ai = {
    enable = mylib.mkBoolOpt false;
    ai-models = mylib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "gpt-4" ];
      description = "List of AI models to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    packages = with pkgs; [
      unstable.ollama
    ];
  };
}
