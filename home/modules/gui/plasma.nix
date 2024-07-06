{ inputs, lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.gui.plasma;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.gui.plasma.enable = mylib.mkBoolOpt true;

  config = mkIf cfg.enable {
    programs.plasma = {
      enable = true;

      workspace = {
        lookAndFeel = "org.kde.breezedark.desktop";
      };

      panels = [
        {
          location = "top";
          widgets = [
            "org.kde.plasma.kickoff"
            "org.kde.plasma.appmenu"
            "org.kde.plasma.panelspacer"
            "org.kde.plasma.pager"
            "org.kde.plasma.systemtray"
            "org.kde.plasma.digitalclock"
          ];
        }
      ];
    };
  };
}
