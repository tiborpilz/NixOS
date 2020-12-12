{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    useGlamor = true;
    desktopManager = {
      plasma5.enable = true;
    };
    windowManager = {
      bspwm.enable = true;
    };
    displayManager = {
      defaultSession = "none+bspwm";
    };
  };
}
