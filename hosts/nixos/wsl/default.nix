{ inputs, ... }:

# NixOS inside WSL2. The nixos-wsl module supplies the interop layer
# (systemd, /mnt/c mounts, the wsl.exe shim); Windows provides the kernel and
# the GUI, so there is no bootloader, hardware-configuration or display server.
{
  imports = [ inputs.nixos-wsl.nixosModules.default ];

  config = {
    wsl.enable = true;
    wsl.defaultUser = "tibor";

    home.enable = true;
    home.graphical = false;

    system.stateVersion = "26.05";
  };
}
