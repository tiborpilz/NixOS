{ lib, inputs, ... }:
let
  installerModule =
    { host, offline }:
    { modulesPath, pkgs, lib, ... }:
    let
      target = inputs.self.nixosConfigurations.${host};
      src = inputs.self;

      disks = target.config.disko.devices.disk or { };
      hasDisko = disks != { };
      devices = lib.mapAttrsToList (_: d: d.device) disks;

      partition = lib.optionalString hasDisko ''
        echo "This ERASES the following disk(s):"
        printf '  %s\n' ${lib.escapeShellArgs devices}
        echo
        lsblk -o NAME,SIZE,MODEL,TRAN || true
        echo
        read -rp 'Type ERASE to continue: ' confirm
        [ "$confirm" = "ERASE" ] || { echo "Aborted."; exit 1; }

        ${target.config.system.build.diskoScript}
      '';

      installCmd =
        if offline
        then "nixos-install --system ${target.config.system.build.toplevel} --no-root-passwd"
        else ''nixos-install --flake "path:${src}#${host}" --no-root-passwd'';

      installer = pkgs.writeShellApplication {
        name = "install-${host}";
        runtimeInputs = [ pkgs.nixos-install-tools pkgs.util-linux ];
        text = ''
          set -euo pipefail
          ${partition}
          ${installCmd}

          echo
          echo "Done. Remove the install media and reboot."
        '';
      };
    in
    {
      imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") ];

      # Without this the minimal ISO has no wifi firmware.
      hardware.enableRedistributableFirmware = true;

      environment.systemPackages = [ installer pkgs.git ];

      isoImage.storeContents =
        lib.optional offline target.config.system.build.toplevel;

      isoImage.contents = [{
        source = src;
        target = "/${host}-config";
      }];

      # baseName is the filename, volumeID the label the OS shows when mounted.
      # image.fileName / isoImage.isoName name neither.
      image.baseName = lib.mkForce "${host}-installer";
      isoImage.volumeID = lib.mkForce "${host}-installer"; # max 32 chars

      services.getty.helpLine = lib.mkAfter ''

        ${host} installer
          1. `nmtui`${lib.optionalString offline " (optional, install works offline)"}
          2. `install-${host}`${lib.optionalString hasDisko "  -- ERASES ${lib.concatStringsSep ", " devices}"}
      '';

      system.stateVersion = "26.05";
    };
in
{
  # Installer ISO for any host: `nix build .#isos.<name>`. Boots to a console
  # with `nmtui` and an `install-<host>` command that partitions using the
  # target's disko config. offline = true embeds the closure (multi-GB image).
  mkIso =
    { host
    , offline ? false
    , system ? "x86_64-linux"
    }:
    (inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [ (installerModule { inherit host offline; }) ];
    }).config.system.build.isoImage;
}
