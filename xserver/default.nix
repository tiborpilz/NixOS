{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    useGlamor = true;
    windowManager = {
      bspwm.enable = true;
      i3.enable = true;
    };
    desktopManager = {
      plasma5.enable = true;
      xterm.enable = true;
      xfce.enable = true;
    };
    displayManager = {
      defaultSession = "plasma5+bspwm";
    };
  };
}
