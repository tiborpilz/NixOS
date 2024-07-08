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
      overrideConfig = true; # Make this config truly declarative

      workspace = {
        lookAndFeel = "org.kde.breezedark.desktop";
        wallpaperPictureOfTheDay = {
          provider = "apod";
        };
      };

      # TODO: move this into its own module
      hotkeys.commands =
        let
          desktops = [1 2 3 4 5];
          desktopCommands = listToAttrs (map (d: {
            name = "desktop-${toString d}";
            value = {
              name = "Switch to Desktop ${toString d}";
              key = "Meta+${toString d}";
              command = "qdbus org.kde.KWin /KWin setCurrentDesktop ${toString d}";
            };
          }) desktops);
        in
          desktopCommands // {
            "konsole" = {
              name = "Open Konsole";
              key = "Meta+Return";
              command = "konsole";
            };
          };

      kwin = {
        virtualDesktops = {
          rows = 1;
          number = 5;
        };
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

    programs.konsole = {
      enable = true;
      profiles.main = {
        extraConfig = {
          General = {
            TerminalMargin = 20;
          };
        };
      };
      defaultProfile = "main";
    };
  };
}
