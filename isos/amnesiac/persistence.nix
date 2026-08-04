# The one thing that survives a reboot: a LUKS partition in the free space on
# the same USB stick the ISO was written to.
#
# `persist-setup` creates it once from the running system. On later boots
# `persist-unlock.service` prompts on tty1 before the desktop starts and
# bind-mounts the directories below; skipping the prompt stays fully amnesic.
{ config, lib, pkgs, ... }:
let
  partLabel = "nixos-persist";
  mapperName = "persist";
  partDevice = "/dev/disk/by-partlabel/${partLabel}";
  mapperDevice = "/dev/mapper/${mapperName}";

  user = config.users.users.nixos.name;
  uid = toString config.users.users.nixos.uid;

  # source under /persist -> target on the live filesystem, mode, owner
  binds = [
    { src = "gnupg"; dst = "/home/${user}/.gnupg"; mode = "0700"; owner = "${uid}:users"; }
    { src = "ssh"; dst = "/home/${user}/.ssh"; mode = "0700"; owner = "${uid}:users"; }
    { src = "password-store"; dst = "/home/${user}/.password-store"; mode = "0700"; owner = "${uid}:users"; }
    { src = "Persistent"; dst = "/home/${user}/Persistent"; mode = "0700"; owner = "${uid}:users"; }
    { src = "nm-connections"; dst = "/etc/NetworkManager/system-connections"; mode = "0700"; owner = "0:0"; }
  ];

  tools = with pkgs; [ cryptsetup util-linux e2fsprogs gptfdisk systemd coreutils ];

  persist-setup = pkgs.writeShellApplication {
    name = "persist-setup";
    runtimeInputs = tools;
    text = ''
      if [ "$(id -u)" -ne 0 ]; then
        echo "persist-setup must run as root (try: sudo persist-setup)" >&2
        exit 1
      fi

      if [ -e ${partDevice} ]; then
        echo "A partition labelled ${partLabel} already exists. Nothing to do."
        echo "To start over, delete it with gparted first."
        exit 1
      fi

      # /iso is the ISO mounted in stage 1; its parent device is the stick we
      # booted from, the only disk we are willing to touch.
      iso_part="$(findmnt -no SOURCE /iso || true)"
      if [ -z "$iso_part" ]; then
        echo "Could not tell which device this system booted from." >&2
        echo "That happens when the ISO was booted from a VM/CD rather than a USB stick." >&2
        exit 1
      fi
      disk="/dev/$(lsblk -no PKNAME "$iso_part")"

      echo "Boot medium: $disk"
      lsblk -o NAME,SIZE,MODEL,TRAN "$disk"
      echo
      echo "This creates a new encrypted partition in the free space after the"
      echo "ISO. Existing partitions are left alone, but rewriting the partition"
      echo "table of a hybrid ISO can break BIOS (non-UEFI) booting of this stick."
      echo
      read -rp 'Type PERSIST to continue: ' confirm
      [ "$confirm" = "PERSIST" ] || { echo "Aborted."; exit 1; }

      # dd leaves the GPT backup header at the end of the *image*, not the end
      # of the stick, so the free space stays invisible until it is moved.
      sgdisk --move-second-header "$disk"
      sgdisk --new=0:0:0 --typecode=0:8309 --change-name=0:${partLabel} "$disk"
      udevadm settle

      part="$(blkid -t PARTLABEL=${partLabel} -o device | head -n1)"
      [ -n "$part" ] || { echo "New partition did not appear." >&2; exit 1; }
      echo "Created $part"

      echo
      echo "Choose a passphrase. It is the only thing protecting the keys you"
      echo "put here, and there is no recovery."
      cryptsetup luksFormat --type luks2 --label ${mapperName} "$part"
      cryptsetup open "$part" ${mapperName}
      mkfs.ext4 -q -L ${mapperName} ${mapperDevice}

      echo
      echo "Done. Reboot and enter the passphrase at the tty1 prompt, or run"
      echo 'sudo persist-unlock now to use it in this session.'
    '';
  };

  persist-unlock = pkgs.writeShellApplication {
    name = "persist-unlock";
    runtimeInputs = tools;
    text = ''
      udevadm settle --timeout=10 || true

      if [ ! -e ${partDevice} ]; then
        exit 0 # no persistent storage on this stick; stay amnesic
      fi

      if [ ! -e ${mapperDevice} ]; then
        opened=0
        for _ in 1 2 3; do
          passphrase="$(systemd-ask-password --timeout=120 \
            "Unlock persistent storage (empty to stay amnesic): ")" || passphrase=""
          if [ -z "$passphrase" ]; then
            echo "Continuing without persistent storage."
            exit 0
          fi
          if printf '%s' "$passphrase" \
            | cryptsetup open --key-file=- ${partDevice} ${mapperName}; then
            opened=1
            break
          fi
          echo "Wrong passphrase."
        done
        unset passphrase
        [ "$opened" -eq 1 ] || { echo "Continuing without persistent storage."; exit 0; }
      fi

      mkdir -p /persist
      mountpoint -q /persist || mount ${mapperDevice} /persist
      chmod 0755 /persist

      bind_dir() {
        src="/persist/$1"; dst="$2"; mode="$3"; owner="$4"
        mkdir -p "$src" "$dst"
        chmod "$mode" "$src"
        chown "$owner" "$src"
        mountpoint -q "$dst" || mount --bind "$src" "$dst"
      }

      ${lib.concatMapStringsSep "\n" (b:
        "bind_dir ${lib.escapeShellArgs [ b.src b.dst b.mode b.owner ]}") binds}

      echo "Persistent storage mounted."
    '';
  };
in
{
  environment.systemPackages = [ persist-setup persist-unlock ];

  # Before SDDM, so the bind mounts are in place by the time the session -- and
  # gpg-agent with it -- starts.
  systemd.services.persist-unlock = {
    description = "Unlock and mount persistent storage";
    wantedBy = [ "multi-user.target" ];
    before = [ "display-manager.service" ];
    after = [ "local-fs.target" "systemd-udevd.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = lib.getExe persist-unlock;
      StandardInput = "tty-force";
      StandardOutput = "tty";
      StandardError = "tty";
      TTYPath = "/dev/tty1";
      TTYReset = true;
      TTYVHangup = true;
    };
  };

  # Close on shutdown rather than leaving the key in the kernel keyring for
  # whatever boots next off the same hardware.
  systemd.services.persist-lock = {
    description = "Close persistent storage";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/true";
      ExecStop = pkgs.writeShellScript "persist-lock" ''
        ${pkgs.util-linux}/bin/umount -R /persist 2>/dev/null || true
        ${pkgs.cryptsetup}/bin/cryptsetup close ${mapperName} 2>/dev/null || true
      '';
    };
  };
}
