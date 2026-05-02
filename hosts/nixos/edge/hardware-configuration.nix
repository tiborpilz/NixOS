{ ... }:
{
  boot.loader.grub.enable = true;
  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "virtio_scsi" "sd_mod" "sr_mod" "ext4" ];
  boot.initrd.kernelModules = [ "nvme" ];
}
