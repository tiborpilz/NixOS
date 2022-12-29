{ inputs, config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.pywal;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.pywal = {
    enable = mylib.mkBoolOpt false;
    enableWallpaper = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable ({
    home.packages = [ pkgs.pywal ];

  } // (mkIf cfg.enableWallpaper {
    modules.xserver.xinitrc.text = ''
          # Load pywal colors
          export WAL_THEME=$(cat "${XDG_CACHE_HOME:-$HOME/.cache}/wal/colors.json")
          wal -i "${XDG_WALLPAPER_DIR:-$HOME/Pictures}" -q
        '';
    })
  );
}
