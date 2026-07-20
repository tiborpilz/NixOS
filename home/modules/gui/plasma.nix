{ inputs, lib, pkgs, config, options, ... }:

with lib;
let
  cfg = config.modules.gui.plasma;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.gui.plasma.enable = mylib.mkBoolOpt true;

  config = mkIf cfg.enable (mkMerge [
    # Hack because we can't be sure that the plasma exists (for instance on darwin)
    (if (hasAttr "plasma" options.programs) then {
      home.packages = [ pkgs.nordic ];

      programs.plasma = {

        enable = true;
        overrideConfig = true; # Make this config truly declarative

        workspace = {
          lookAndFeel = "org.kde.breezedark.desktop";
          colorScheme = "Nordic";
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

        # Device entries only apply on machines with matching hardware
        input.touchpads = [
          {
            # thinkyMcThinkpad
            name = "ETPS/2 Elantech Touchpad";
            vendorId = "0002";
            productId = "000e";
            enable = true;
            naturalScroll = true;
          }
        ];

        shortcuts = {
          kwin = {
            "Overview" = "Meta+W";
            "Grid View" = "Meta+G";
            "Show Desktop" = "Meta+D";
            "Edit Tiles" = "Meta+T";
            "Window Fullscreen" = "Meta+Ctrl+F";
          };
          plasmashell = {
            "manage activities" = "Meta+Q";
            "next activity" = "Meta+A";
            "previous activity" = "Meta+Shift+A";
            "show-on-mouse-pos" = "Meta+V"; # clipboard popup
          };
          org_kde_powerdevil = {
            "powerProfile" = [ "Battery" "Meta+B" ];
          };
        };

        configFile = {
          kdeglobals = {
            KDE.widgetStyle = "Breeze";
          };
          kwinrc = {
            Effect-overview.BorderActivate = 9; # Overview on screen-edge
            Plugins.slideEnabled = false; # no desktop-slide animation
          };
          ksplashrc.KSplash = {
            Engine = "none";
            Theme = "None";
          };
          kwalletrc.Wallet."First Use" = false;
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
            height = 32;
            floating = true;
          }
        ];
      };
    } else { })

    (if (hasAttr "konsole" options.programs) then {
      programs.konsole = {
        enable = true;
        profiles.main = {
          colorScheme = "Nordic";
          extraConfig = {
            General = {
              TerminalMargin = 20;
            };
          };
        };
        defaultProfile = "main";
      };
    } else { })
  ]);
}
