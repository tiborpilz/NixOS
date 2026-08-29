{
  config,
  lib,
  pkgs,
  inputs,
  options,
  ...
}:

with lib;
let
  cfg = config.modules.firefox;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  # plasma-manager is only in home-manager.sharedModules on NixOS hosts, so the
  # options probe doubles as the Darwin guard — same trick as gui/plasma.nix.
  plasmaActive = config.modules.gui.plasma.enable && hasAttr "plasma" options.programs;

  # Firefox application extension path — where profile-scoped add-ons live.
  appExtensionPath = "extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}";

  # Minimal add-on package consumable by home-manager's
  # programs.firefox.profiles.<name>.extensions.packages: an xpi placed at
  # share/mozilla/<appExtensionPath>/<addonId>.xpi. Pinned to specific AMO
  # versions for reproducibility (no NUR input needed).
  mkFirefoxAddon =
    {
      addonId,
      version,
      url,
      hash,
    }:
    pkgs.runCommand "firefox-addon-${version}-${removeSuffix ".xpi" (baseNameOf url)}"
      {
        passthru = {
          inherit addonId version;
        };
      }
      ''
        mkdir -p "$out/share/mozilla/${appExtensionPath}"
        cp "${pkgs.fetchurl { inherit url hash; }}" \
          "$out/share/mozilla/${appExtensionPath}/${addonId}.xpi"
      '';

  # scifox profile add-ons (github.com/scientiac/scifox)
  firefoxAddons = {
    tridactyl = mkFirefoxAddon {
      addonId = "tridactyl.vim@cmcaine.co.uk";
      version = "1.25.0";
      url = "https://addons.mozilla.org/firefox/downloads/file/4988638/tridactyl_vim-1.25.0.xpi";
      hash = "sha256-RvTexbgcCKaIxwShsup7RfRLOkocQpX5kcq/+2cPGBY=";
    };
    sidebery = mkFirefoxAddon {
      addonId = "{3c078156-979c-498b-8990-85f7987dd929}";
      version = "5.6.1";
      url = "https://addons.mozilla.org/firefox/downloads/file/4903712/sidebery-5.6.1.xpi";
      hash = "sha256-6KCktVarfdU2iXwYFq+dCRgDAiMGjqZoOgQ3YQOmyvI=";
    };
    adaptive-tab-bar-colour = mkFirefoxAddon {
      addonId = "ATBC@EasonWong";
      version = "4.1.0";
      url = "https://addons.mozilla.org/firefox/downloads/file/4933754/adaptive_tab_bar_colour-4.1.0.xpi";
      hash = "sha256-gA/htClqWym3Ik5Yd5W84VDlt5dFwVn+nJj/cdiIPJ8=";
    };
  };
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

      profiles = {
        # The existing everyday profile — left alone (no prefs, no chrome),
        # only registered so it stays the browser default.
        default = {
          id = 1;
          name = "default";
          path = "t9o0f0h6.default";
          isDefault = true;
        };

        # Minimalist scifox profile — launch with `firefox -P main`
        main = {
          id = 0;
          isDefault = false;
          extensions.packages = [
            firefoxAddons.tridactyl
            firefoxAddons.sidebery
            firefoxAddons.adaptive-tab-bar-colour
          ];

          # scifox user.js (github.com/scientiac/scifox) — prefs required for
          # userChrome/userContent, plus its UI/perf tweaks.
          settings = {
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            "svg.context-properties.content.enabled" = true;
            "layers.acceleration.force-enabled" = true;
            "gfx.webrender.all" = true;
            "browser.startup.preXulSkeletonUI" = false;
            # Enable profile-seeded extensions without manual approval
            "extensions.autoDisableScopes" = 0;
            # Keep the newtab search bar instead of jumping to the urlbar
            # (the urlbar is faded out by userChrome).
            "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
          };
          userChrome = builtins.readFile ./chrome/userChrome.css;
          userContent = builtins.readFile ./chrome/userContent.css;
        };
      };
    };

    xdg = {
      desktopEntries.firefox-minimal = {
        name = "Firefox (minimal)";
        genericName = "Web Browser";
        comment = "Minimalist scifox profile with tridactyl";
        # --no-remote so it runs alongside a normal Firefox instance;
        # no mimeType so the default browser stays the regular profile.
        exec = "firefox -P main --no-remote %U";
        icon = "firefox";
        type = "Application";
        categories = [
          "Network"
          "WebBrowser"
        ];
      };

      configFile = {
        "tridactyl/tridactylrc".text = ''
          " Minimalist grayscale theme matching the scifox userChrome.
          " Loaded from ~/.config/tridactyl/themes/ via the native messenger.
          colourscheme grayscale-scifox
        '';

        "tridactyl/themes/grayscale-scifox.css".source = ./tridactyl/grayscale-scifox.css;
      };
    };
  };
}
