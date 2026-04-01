{ inputs, config, lib, pkgs, ... }:

# pay-respects is a replacement for thefuck. It suggests fixes for mistakes in previous terminal commands.
with lib;
let
  cfg = config.modules.shell.pay-respects;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.shell.pay-respects = {
    enable = mylib.mkBoolOpt false;
    alias = mkOption {
      type = types.str;
      default = "heck";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.pay-respects # mistake fix suggestions
      pkgs.nix-index # look for packages
    ];
    modules.shell.zsh.rcInit = ''
      eval "$(${pkgs.pay-respects}/bin/pay-respects zsh --alias ${cfg.alias})"
    '';
  };
}
