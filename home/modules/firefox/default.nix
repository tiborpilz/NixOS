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
  plasmaActive = config.modules.gui.plasma.enable && hasAttr "plasma" options.programs;
  firefoxAddons = inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
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
        default = {
          id = 1;
          name = "default";
          path = "t9o0f0h6.default";
          isDefault = true;
        };

        main = {
          id = 0;
          isDefault = false;
          extensions.packages = [
            firefoxAddons.tridactyl
            firefoxAddons.sidebery
            firefoxAddons.adaptive-tab-bar-colour
            firefoxAddons.ublock-origin
            firefoxAddons.bitwarden
            firefoxAddons.reddit-enhancement-suite
          ];

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
          # Paths rather than strings: home-manager writes a non-string as
          # `source`, so these land as symlinks to the working tree. Firefox
          # reads them once at startup, so iterating is edit + restart.
          userChrome = config.lib.file.mkOutOfStoreSymlink
            "${config.home.homeDirectory}/Code/nixos/home/config/firefox/chrome/userChrome.css";
          userContent = config.lib.file.mkOutOfStoreSymlink
            "${config.home.homeDirectory}/Code/nixos/home/config/firefox/chrome/userContent.css";
        };
      };
    };

    xdg = {
      desktopEntries = mkIf pkgs.stdenv.isLinux {
        firefox-minimal = {
          name = "Firefox (minimal)";
          genericName = "Web Browser";
          comment = "Minimalist scifox profile with tridactyl";
          # --no-remote so it runs alongside a normal Firefox instance;
          exec = "firefox -P main --no-remote %U";
          icon = "firefox";
          type = "Application";
          categories = [
            "Network"
            "WebBrowser"
          ];
        };
      };

      configFile = {
        "tridactyl/tridactylrc".text = ''
          " Theme matching the scifox-derived userChrome.
          " Loaded from ~/.config/tridactyl/themes/ via the native messenger.
          colourscheme hush

          " The theme reserves a favicon column on tab rows, and draws a
          " glyph in it for sources the API gives no icon for.
          set completions.Tab.showFavicons true

          " Favicons on history and bookmark completion rows, which the
          " completion API gives no icon for. `-r` resolves relative to this
          " file and re-reads on every call, so the scripts can be edited
          " without a rebuild.
          " autocmds are keyed by event and pattern, so everything that runs
          " on DocStart has to share one script — a second one on the same
          " pattern replaces this rather than adding to it.
          autocmd TriStart .* jsb -r favicons-seed.js
          autocmd DocStart .* js -r hush.js

          " Let Ctrl+E reach Sidebery's sidebar toggle (tridactyl normally
          " binds <C-e> to scrollline 10 and swallows it).
          unbind --mode=normal <C-e>
        '';

        "tridactyl/themes/hush.css".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/tridactyl/hush.css";
        "tridactyl/hush.js".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/tridactyl/hush.js";
        "tridactyl/favicons-seed.js".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/tridactyl/favicons-seed.js";
      };
    };

    # Adaptive Tab Bar Colour declares http/https as optional host
    # permissions, and installing an add-on by dropping its xpi in the profile
    # never runs the grant flow.
    home.activation.grantAdaptiveTabBarHostAccess =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        prefs="${config.home.homeDirectory}/.config/mozilla/firefox/main/extension-preferences.json"
        grant='{"permissions":[],"origins":["http://*/*","https://*/*"],"data_collection":[]}'
        jq="${pkgs.jq}/bin/jq"
        id="${firefoxAddons.adaptive-tab-bar-colour.addonId}"
        if [ ! -e "$prefs" ]; then
          mkdir -p "$(dirname "$prefs")"
          "$jq" -n --arg id "$id" --argjson g "$grant" '{($id): $g}' > "$prefs"
        elif ! "$jq" -e --arg id "$id" '.[$id].origins // [] | index("https://*/*")' "$prefs" >/dev/null; then
          tmp="$(mktemp)"
          "$jq" --arg id "$id" --argjson g "$grant" '.[$id] = $g' "$prefs" > "$tmp" \
            && mv "$tmp" "$prefs"
        fi
      '';

    home.file.".config/mozilla/firefox/main/extension-settings.json" = {
      force = true;
      text = builtins.toJSON {
        version = 3;
        commands = {
          activate.precedenceList = [
            {
              id = firefoxAddons.sidebery.addonId;
              installDate = 1000;
              value.shortcut = "Alt+A";
              enabled = true;
            }
          ];
        };
      };
    };
  };
}
