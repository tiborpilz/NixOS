{ config, lib, pkgs, inputs, options, ... }:

with lib;
let
  cfg = config.modules.firefox;
  mylib = import ../../lib { inherit inputs lib pkgs; };
  # plasma-manager is only in home-manager.sharedModules on NixOS hosts, so the
  # options probe doubles as the Darwin guard — same trick as gui/plasma.nix.
  plasmaActive = config.modules.gui.plasma.enable && hasAttr "plasma" options.programs;
in
{
  options.modules.firefox = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      configPath = ".config/mozilla/firefox";
      nativeMessagingHosts = [
        pkgs.tridactyl-native
      ] ++ optional plasmaActive pkgs.kdePackages.plasma-browser-integration;
    };

    xdg.configFile."tridactyl/tridactylrc".text = ''
      colourscheme --url https://raw.githubusercontent.com/bezmi/base16-tridactyl/master/base16-grayscale-dark.css grayscale-dark
    '';
  };
}
