{ inputs, config, options, pkgs, lib, ... }:

let
  cfg = config.modules.alacritty;
in
with lib;
{
  # options.modules.gui.alacritty = {
  #   enable = mkEnableOption "Alacritty terminal emulator";

  #   config = mkOption {
  #     type = types.attrs;
  #     default = {};
  #     description = "Alacritty configuration";
  #   };
  # };

  # config = mkIf cfg.enable {
  #   programs.alacritty = {
  #     enable = true;
  #   };

  #   # TODO
  #   settings = mkOption {
  #     type = types.attrs;
  #     default = {};
  #     description = "Alacritty configuration";
  #   };
  # };
}
