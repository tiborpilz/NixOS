{ inputs, lib, pkgs, config, options, ... }:

with lib;
let
  cfg = config.modules.gui.plasma;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  desktops = [ 1 2 3 4 5 ];
in
{
  options.modules.gui.plasma.enable = mylib.mkBoolOpt true;

  config = mkIf cfg.enable (mkMerge [
    # Hack because we can't be sure that the plasma exists (for instance on darwin)
    (if (hasAttr "plasma" options.programs) then {
      home.packages = [ pkgs.nordic pkgs.nixos-icons ];

      programs.plasma = {

        enable = true;
        overrideConfig = true; # Make this config truly declarative

        workspace = {
          lookAndFeel = "org.kde.breezedark.desktop";
          colorScheme = "NordicDarker";
          wallpaper = "${pkgs.kdePackages.plasma-workspace-wallpapers}/share/wallpapers/Mountain/";
        };

        # Desktop switching (Meta+1..5) is bound to KWin's native action in
        # shortcuts.kwin below. Command hotkeys launch a process, which makes
        # KDE show app-startup feedback (spinning cursor + a launch entry), so
        # they're reserved for actually launching programs.
        hotkeys.commands = {
          "kitty" = {
            name = "Open Kitty";
            key = "Meta+Return";
            command = "kitty";
          };
        };

        kwin = {
          virtualDesktops = {
            rows = 1;
            number = 5;
          };
        };

        krunner.position = "center";

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
          } // listToAttrs (map
            (d: {
              name = "Switch to Desktop ${toString d}";
              value = "Meta+${toString d}";
            })
            desktops);
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
              {
                kickoff.icon = "nix-snowflake-white";
              }
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
