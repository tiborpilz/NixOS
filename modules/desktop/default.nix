{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't have more than one desktop environment enabled at a time";
      }
      {
        assertion =
          let srv = config.services;
          in srv.xserver.enable ||
            srv.sway.enable ||
            !(anyAttrs
              (n: v: isAttrs v &&
                anyAttrs (n: v: isAttrs v && v.enable))
              cfg);
        message = "Can't enable a desktop app without a desktop environment";
      }
    ];

    user.packages = with pkgs; [
      feh
      xclip
      xdotool
      xorg.xwininfo
    ];
  };
}
