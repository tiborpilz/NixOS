{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.editors.zed;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.editors.zed = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.unstable.zed-editor ];

    xdg.configFile."zed/settings.json".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/zed/settings.json";
    xdg.configFile."zed/keymap.json".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/zed/keymap.json";
  };
}
