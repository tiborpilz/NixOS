{ config, lib, pkgs, ...}:

with lib;
let
  cfg = config.modules.shell.atuin;
in
{
  options.modules.shell.atuin = {
    enable = lib.mkEnableOption "atuin";
  };

  config = mkIf cfg.enable {
    programs.atuin = {
      enable = true;
      settings = {
        enter_accept = false;
      };
    };

    modules.shell.zsh = {
      fpathDirs = "${pkgs.atuin}/share/zsh/site-functions";
      rcInit = "eval \"\$(${pkgs.atuin}/bin/atuin init zsh --disable-up-arrow)\"";
    };
  };
}
