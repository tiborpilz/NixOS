{ pkgs, lib, config, ... }:
with lib;
{
  # Compatibility workaround, some home modules set systemd which is not available on darwin
  options = {
    systemd = mkOption {
      type = types.attrs;
      default = { };
    };
  };

  config = {
    homebrew.enable = true;

    homebrew.taps = [
      "felixkratz/formulae" #sketchybar
      "koekeishiya/formulae" #yabai/skhd
    ];

    homebrew.casks = [
      "flutter"
      "podman-desktop"
      "android-sdk"
      "cool-retro-term"
      "mactex"
    ];

    homebrew.brews = [
      "lima" # docker-desktop / vm alternative
      "podman" # docker alternative
      "sketchybar"
      "yabai"
      "skhd"
      "ddcctl" # brightness control
    ];

    services.nix-daemon.enable = true;
    services.karabiner-elements.enable = true;

    programs.zsh.enable = true;

    users.users."tibor.pilz" = {
      shell = pkgs.zsh;
      description = "Tibor Pilz";
      home = "/Users/tibor.pilz";
    };

    system.activationScripts.applications.text = pkgs.lib.mkForce ''
      echo "setting up ~/Applications..." >&2
      rm -rf ~/Applications/Nix\ Apps
      mkdir -p ~/Applications/Nix\ Apps
      for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
        src="$(/usr/bin/stat -f%Y "$app")"
        cp -r "$src" ~/Applications/Nix\ Apps
      done
    '';
  };
}
