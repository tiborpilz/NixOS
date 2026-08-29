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
          userChrome = builtins.readFile ./chrome/userChrome.css;
          userContent = builtins.readFile ./chrome/userContent.css;
        };
      };
    };

    xdg = {
      # home-manager asserts xdg.desktopEntries is linux-only, so on Darwin
      # this is an eval error rather than a no-op. There is no .desktop
      # equivalent there anyway — the profile is launched by its own means.
      desktopEntries = mkIf pkgs.stdenv.isLinux {
        firefox-minimal = {
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
      };

      configFile = {
        "tridactyl/tridactylrc".text = ''
          " Theme matching the scifox-derived userChrome.
          " Loaded from ~/.config/tridactyl/themes/ via the native messenger.
          colourscheme hush

          " The theme reserves a favicon column on tab rows, and draws a
          " glyph in it for sources the API gives no icon for.
          set completions.Tab.showFavicons true

          " Let Ctrl+E reach Sidebery's sidebar toggle (tridactyl normally
          " binds <C-e> to scrollline 10 and swallows it).
          unbind --mode=normal <C-e>
        '';

        # Out of store so the theme can be iterated on without a rebuild.
        # `:source` re-runs this rc, and `colourscheme` re-reads the file from
        # disk every time — but it then sets `theme` to the value it already
        # had, so nothing re-applies the new CSS until the page reloads.
        # The loop is therefore: edit, `:source`, reload the page.
        # Changes to the rc itself still need a switch.
        "tridactyl/themes/hush.css".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/tridactyl/hush.css";
      };
    };

    # Adaptive Tab Bar Colour declares http/https as *optional* host
    # permissions, and installing an add-on by dropping its xpi in the profile
    # never runs the grant flow — so it starts with no host access and
    # silently never reads a page's colour. extensions.originControls.
    # grantByDefault is already true and does not cover this path.
    #
    # The grant lives in extension-preferences.json, which Firefox also writes
    # at runtime (it holds Sidebery's <all_urls> and the built-ins' internal
    # flags), so merge the one key rather than declaring the whole file.
    # Applies on the next Firefox start.
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

    # Declarative extension shortcut overrides. Firefox keeps these in the
    # profile's extension-settings.json (ExtensionSettingsStore, type
    # "commands" — see ExtensionShortcuts.sys.mjs). KDE grabs Alt+Space
    # (KRunner), so Sidebery's "activate" is rebound to Alt+A.
    # force=true makes this file authoritative: shortcuts changed in the
    # about:addons GUI are reverted on switch — change them here instead.
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
