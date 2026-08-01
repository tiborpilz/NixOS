{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.keyd;

  # Hold AltGr for vim-style arrow keys
  altgrArrows = {
    altgr = {
      h = "left";
      j = "down";
      k = "up";
      l = "right";
    };
  };
in
{
  options.modules.desktop.keyd = {
    enable = mkBoolOpt false;
    swapEscapeInternal = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.keyd = {
      enable = true;
      keyboards = {
        default = {
          ids = [ "*" ];
          settings = altgrArrows;
        };
      } // optionalAttrs cfg.swapEscapeInternal {
        internal = {
          # Internal keyboard (I swap caps and escape on keyboard firmware level otherwise)
          ids = [ "0001:0001" ];
          settings = recursiveUpdate altgrArrows {
            main = {
              capslock = "esc";
              esc = "capslock";
            };
          };
        };
      };
    };
  };
}
