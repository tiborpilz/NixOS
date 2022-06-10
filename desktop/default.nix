{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    useGlamor = true;
    desktopManager = {
      plasma5.enable = true;
    };
    displayManager = {
      defaultSession = "plasma5";
    };
  };
}
