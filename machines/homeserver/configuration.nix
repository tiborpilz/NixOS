{ pkgs, lib, config, modulesPath, ...}:

with lib;
{
  imports = [
    # (modulesPath + "/nixos/modules/profiles/qemu-guest.nix")
    # (modulesPath + "/nixos/modules/virtualisation/qemu-vm.nix")
    ./services/tandoor.nix
    ./services/paperless-ng.nix
    ./services/media/media.nix
  ];

  config = {
    virtualisation.memorySize = 32768;
    virtualisation.cores = 8;
    virtualisation.diskSize = 32768;

    virtualisation.qemu = {
      networkingOptions = ["-nic bridge,br=virbr0,model=virtio-net-pci"];
    };

    boot.loader.grub.enable = true;

    fileSystems = {
      "/".label = "nixos-root";
    };

    networking.firewall.enable = false;

    services.qemuGuest.enable = true;

    services.openssh.enable = true;
    services.openssh.permitRootLogin = "yes";


    users.extraUsers.root.password = "";
    users.mutableUsers = false;

    users.users.tibor = {
      uid = 1000;
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      password = "password";
    };

    virtualisation.oci-containers.backend = "podman";
    system.stateVersion = "22.05";
  };
}
