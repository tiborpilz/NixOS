{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot = {

    consoleLogLevel = 0;
    loader = {
      efi = {
        efiSysMountPoint = "/boot";
      };

      grub = {
        enable = true;
        device = "nodev";
        efiSupport= true;
        version = 2;
        gfxmodeEfi = "1920x1080";
        gfxpayloadEfi = "keep";
      };

    };

    kernelModules = ["kvm-amd"];
    kernelParams = ["amd_iommu=on"];
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      qemuOvmf = true;
    };
  };

  console = {
    earlySetup = true;
  };

  networking.hostName = "workyMcNixStation";
  networking.useDHCP = true;
}
