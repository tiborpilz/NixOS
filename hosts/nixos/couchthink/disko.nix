{ ... }:
# ThinkPad X240: single 2.5" SATA drive, UEFI (no EF02/GRUB unlike edge).
#
# `device` must not become a function argument — that breaks
# system.build.diskoScript, which the installer and nixos-anywhere both use.
# Confirm with `lsblk` before running anything that formats; if the OS lives on
# an mSATA card rather than the 2.5" bay, change it here.
{
  disko.devices.disk.main = {
    type = "disk";
    device = "/dev/sda";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          size = "512M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
}
