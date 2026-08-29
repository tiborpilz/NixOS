{ config, lib, pkgs, inputs, options, ... }:

with lib;
let
  cfg = config.modules.firefox;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
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

      profiles.main = {
        id = 0;
        extensions = [
          # Tridactyl: keyboard-driven browsing (native host installed below)
          {
            id = "tridactyl.vim@cmcaine.co.uk";
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/tridactyl-vim/latest.xpi";
          }
          # Sidebery (V5): tabs in the auto-hiding sidebar, per scifox
          {
            id = "{3c078156-979c-498b-8990-85f7987dd929}";
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/sidebery/latest.xpi";
          }
          # Adaptive Tab Bar Colour: toolbar adapts to page background —
          # required for scifox's adaptive-color look
          {
            id = "ATBC@EasonWong";
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/adaptive-tab-bar-colour/latest.xpi";
          }
        ];
        # scifox user.js (github.com/scientiac/scifox) — prefs required for
        # userChrome/userContent, plus its UI/perf tweaks.
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "svg.context-properties.content.enabled" = true;
          "layers.acceleration.force-enabled" = true;
          "gfx.webrender.all" = true;
          "browser.startup.preXulSkeletonUI" = false;
          # Keep the newtab search bar instead of jumping to the urlbar
          # (the urlbar is faded out by userChrome).
          "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
        };
        userChrome = builtins.readFile ./chrome/userChrome.css;
        userContent = builtins.readFile ./chrome/userContent.css;
      };
    };

    xdg.configFile = {
      "tridactyl/tridactylrc".text = ''
        " Minimalist grayscale theme matching the scifox userChrome.
        " Loaded from ~/.config/tridactyl/themes/ via the native messenger.
        colourscheme grayscale-scifox
      '';

      "tridactyl/themes/grayscale-scifox.css".source = ./tridactyl/grayscale-scifox.css;
    };
  };
}
