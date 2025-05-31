{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.firefox;
  mylib = import ../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.firefox = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      profiles = {
        nixboi = {
          force = true;
        };
      };

      nativeMessagingHosts = [
        pkgs.tridactyl-native
      ];
    };

    xdg.configFile."tridactyl/tridactylrc".text = ''
      colourscheme --url https://raw.githubusercontent.com/bezmi/base16-tridactyl/master/base16-grayscale-dark.css grayscale-dark
    '';
  };


  # config = mkIf cfg.enable {
  #   programs.firefox = {
  #     enable = true;
  #     package = pkgs.firefox-unwrapped;
  #     nativeMessagingHosts = [
  #       pkgs.tridactyl-native
  #     ];
  #   };

    # xdg.configFile."tridactyl/tridactylrc".text = ''
    #   colourscheme --url https://raw.githubusercontent.com/bezmi/base16-tridactyl/master/base16-grayscale-dark.css grayscale-dark
    # '';
}
