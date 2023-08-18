{ inputs, config, options, pkgs, lib, ... }:

let
  cfg = config.modules.terminal.kitty;
in
with lib;
{
  options.modules.terminal.kitty = {
    enable = mkEnableOption "Kitty terminal emulator";
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        package = pkgs.fira-code;
        size = 16;
        name = "Fira Code";
      };
      theme = "Space Gray Eighties";
    };
  };
}
