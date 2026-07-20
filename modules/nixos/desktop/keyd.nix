{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.keyd;
in
{
  options.modules.desktop.keyd = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.keyd = {
      enable = true;
      keyboards.default = {
        ids = [ "*" ];
        settings = {
          # Hold AltGr for vim-style arrow keys
          altgr = {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
          };
        };
      };
    };
  };
}
