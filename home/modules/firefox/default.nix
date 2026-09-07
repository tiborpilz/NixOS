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

  # macOS has no .desktop files. The equivalent of the firefox-minimal entry
  # below is an app bundle, and home-manager links every .app it finds under a
  # home.packages output into ~/Applications/Home Manager Apps.
  #
  # The launcher execs Mozilla's build in /Applications rather than the nix
  # one, so a single Firefox version opens every profile under the shared
  # profile root. Tridactyl still works either way, because native messaging
  # manifests go to ~/Library/Application Support/Mozilla/NativeMessagingHosts
  # rather than into a browser package.
  firefoxBin = "/Applications/Firefox.app/Contents/MacOS/firefox";

  # Only for the icon, which has to exist at build time.
  firefoxApp = "${config.programs.firefox.finalPackage}/Applications/Firefox.app";

  firefoxMinimalLauncher = pkgs.writeShellScript "firefox-minimal" ''
    exec "${firefoxBin}" -P main --no-remote "$@"
  '';

  firefoxMinimalPlist = pkgs.writeText "firefox-minimal-Info.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleExecutable</key><string>firefox-minimal</string>
      <key>CFBundleIconFile</key><string>firefox.icns</string>
      <key>CFBundleIdentifier</key><string>org.nixos.firefox-minimal</string>
      <key>CFBundleName</key><string>Firefox Minimal</string>
      <key>CFBundlePackageType</key><string>APPL</string>
      <key>LSMinimumSystemVersion</key><string>10.15.0</string>
      <key>NSHighResolutionCapable</key><true/>
    </dict>
    </plist>
  '';

  firefoxMinimalApp = pkgs.runCommandLocal "firefox-minimal-app" { } ''
    contents="$out/Applications/Firefox Minimal.app/Contents"
    mkdir -p "$contents/MacOS" "$contents/Resources"
    cp ${firefoxMinimalPlist} "$contents/Info.plist"
    cp ${firefoxMinimalLauncher} "$contents/MacOS/firefox-minimal"
    chmod +x "$contents/MacOS/firefox-minimal"
    ln -s ${firefoxApp}/Contents/Resources/firefox.icns "$contents/Resources/firefox.icns"
  '';
in
{
  options.modules.firefox = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      # macOS Firefox only ever reads Library/Application Support/Firefox;
      # there is no env var to redirect it. Pointing this at the Linux
      # XDG path on darwin hides every profile declared here.
      configPath =
        if pkgs.stdenv.isDarwin then
          "Library/Application Support/Firefox"
        else
          ".config/mozilla/firefox";
      nativeMessagingHosts = [
        pkgs.tridactyl-native
      ] ++ optional plasmaActive pkgs.kdePackages.plasma-browser-integration;

      profiles = {
        default = {
          id = 1;
          name = "default";
          path = "t9o0f0h6.default";
          isDefault = !pkgs.stdenv.isDarwin;
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
      }
      // optionalAttrs pkgs.stdenv.isDarwin {
        # Pre-existing profile this Mac's Firefox has always used. Declared so
        # home-manager's generated profiles.ini keeps listing it.
        default-release = {
          id = 2;
          name = "default-release";
          path = "yn3nwp84.default-release";
          isDefault = true;
        };
      };
    };

    home.packages = optional pkgs.stdenv.isDarwin firefoxMinimalApp;

    # Spotlight indexes /Applications but not the home directory on this
    # machine, so ~/Applications/Home Manager Apps is never searchable. Copy
    # the bundle where Spotlight already looks. /Applications is group-writable
    # by admin, so this needs no sudo.
    home.activation.installFirefoxMinimalApp = mkIf pkgs.stdenv.isDarwin (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        target="/Applications/Firefox Minimal.app"
        if [ -e "$target" ] && ! grep -q org.nixos.firefox-minimal "$target/Contents/Info.plist" 2>/dev/null; then
          echo "skipping $target: exists and is not the bundle built here" >&2
        else
          $DRY_RUN_CMD rm -rf "$target"
          $DRY_RUN_CMD cp -fHRL "${firefoxMinimalApp}/Applications/Firefox Minimal.app" /Applications/
          $DRY_RUN_CMD chmod -R +w "$target"
        fi
      ''
    );

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
          " load custom hush colorscheme
          colourscheme hush

          " The theme reserves a favicon column on tab rows, and draws a
          " glyph in it for sources the API gives no icon for.
          set completions.Tab.showFavicons true

          " Favicons on history and bookmark completion rows, which the
          " completion API gives no icon for. `-r` resolves relative to this
          " file and re-reads on every call, so the scripts can be edited
          " without a rebuild.
          " autocmds are keyed by event *and* pattern: a second entry on the
          " same pair replaces the first rather than adding to it. Tridactyl's
          " own default is `TriStart .* source_quiet`, which is what re-reads
          " this file at browser start — claiming that pair for anything else
          " silently stops the rc from ever being sourced again. So it is
          " re-declared here, and startup work runs as a plain command.
          autocmd TriStart .* source_quiet

          " Seeds the favicon cache; runs each time this file is sourced.
          jsb -r favicons-seed.js

          " Everything that runs per page shares one script, for the same
          " event+pattern reason.
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
        prefs="${config.home.homeDirectory}/${config.programs.firefox.profilesPath}/main/extension-preferences.json"
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

    home.file."${config.programs.firefox.profilesPath}/main/extension-settings.json" = {
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
