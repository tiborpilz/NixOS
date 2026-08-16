set dotenv-load
set dotenv-filename := "servers.env"

is_nixos := shell('if [ -f /etc/os-release ] && grep -q "NixOS" /etc/os-release; then echo true; else echo false; fi')

# Show this help
help:
  just --list

# Deploy a server. Qualify the name to reach it by another route:
# `klaus` uses KLAUS from servers.env, `klaus.tailscale` uses KLAUS_TAILSCALE.
deploy server mode="switch":
  #!/usr/bin/env bash
  set -euo pipefail
  name="{{server}}"
  var="$(printf '%s' "${name//./_}" | tr '[:lower:]-' '[:upper:]_')"
  addr="${!var:?$var is not set in servers.env}"
  portvar="${var}_PORT"
  NIX_SSHOPTS="-p ${!portvar:-22}" \
  nix run nixpkgs#nixos-rebuild -- \
    {{ if mode == "dry" { "dry-run"} else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --flake ".#${name%%.*}" \
    --target-host "root@$addr" \
    --build-host "root@$addr" \
    --fast

# Deploy, building locally and pushing the closure (for slow targets)
deploy-push server address="" mode="switch":
  NIX_SSHOPTS="-i ~/.ssh/id" \
  nix run nixpkgs#nixos-rebuild -- \
    {{ if mode == "dry" { "dry-run"} else if mode == "switch" { "switch" } else { error("Unknown Mode") } }} \
    --flake .#{{server}} \
    --target-host root@{{ if address == "" { env_var(uppercase(server)) } else { address } }} \
    --fast

# Deploy a host with deploy-rs (SSH transport from ~/.ssh/config)
deploy-rs node="klaus" *ARGS:
  nix run github:serokell/deploy-rs -- .#{{node}} {{ARGS}}

# Switch the home-manager configuration
homemanager:
  nh home switch

# Automatically switch the local configuration based on the system type
switch:
  {{ if is_nixos == "true" { "sudo nixos-rebuild switch --flake ." } else { "nh home switch ." } }}

# Build an ISO (installer or live) into ./result
iso name:
  nix build .#isos.{{name}}

# Write a built ISO to a USB stick -- this DESTROYS everything on it
iso-write name device:
  #!/usr/bin/env bash
  set -euo pipefail
  nix build .#isos.{{name}} --out-link result-iso
  echo "About to overwrite {{device}}:"
  lsblk -o NAME,SIZE,MODEL,TRAN {{device}}
  read -rp 'Type ERASE to continue: ' confirm
  [ "$confirm" = "ERASE" ] || { echo "Aborted."; exit 1; }
  sudo dd if=result-iso/iso/*.iso of={{device}} bs=4M conv=fsync status=progress
  sync

# Boot an ISO in a throwaway UEFI VM (no persistence -- it looks like a CD)
vm-iso name mem="4G":
  #!/usr/bin/env bash
  set -euo pipefail
  nix build .#isos.{{name}} --out-link result-iso
  iso=$(echo result-iso/iso/*.iso)
  ovmf=$(nix build --no-link --print-out-paths nixpkgs#OVMF.fd)
  vars=$(mktemp -t OVMF_VARS.XXXXXX.fd)
  trap 'rm -f "$vars"' EXIT
  install -m600 "$ovmf/FV/OVMF_VARS.fd" "$vars"
  nix run nixpkgs#qemu -- \
    -enable-kvm -cpu host -smp 4 -m {{mem}} \
    -drive if=pflash,format=raw,readonly=on,file="$ovmf/FV/OVMF_CODE.fd" \
    -drive if=pflash,format=raw,file="$vars" \
    -cdrom "$iso" -boot d \
    -vga virtio -display gtk \
    -netdev user,id=net0 -device virtio-net-pci,netdev=net0

# Boot an ISO off a simulated USB stick in .vm/, so persist-setup can run
vm-usb name size="16G" mem="4G":
  #!/usr/bin/env bash
  set -euo pipefail
  # The stick image survives between runs, which is what makes persistence
  # testable; delete .vm/<name>-usb.img to start from a fresh stick.
  nix build .#isos.{{name}} --out-link result-iso
  iso=$(echo result-iso/iso/*.iso)
  img=".vm/{{name}}-usb.img"
  mkdir -p .vm
  if [ ! -f "$img" ]; then
    echo "Creating $img ({{size}}) from $iso"
    truncate -s {{size}} "$img"
    dd if="$iso" of="$img" bs=4M conv=notrunc status=progress
  fi
  ovmf=$(nix build --no-link --print-out-paths nixpkgs#OVMF.fd)
  vars=$(mktemp -t OVMF_VARS.XXXXXX.fd)
  trap 'rm -f "$vars"' EXIT
  install -m600 "$ovmf/FV/OVMF_VARS.fd" "$vars"
  nix run nixpkgs#qemu -- \
    -enable-kvm -cpu host -smp 4 -m {{mem}} \
    -drive if=pflash,format=raw,readonly=on,file="$ovmf/FV/OVMF_CODE.fd" \
    -drive if=pflash,format=raw,file="$vars" \
    -drive if=none,id=usbstick,format=raw,file="$img" \
    -device qemu-xhci,id=xhci -device usb-storage,bus=xhci.0,drive=usbstick \
    -vga virtio -display gtk \
    -netdev user,id=net0 -device virtio-net-pci,netdev=net0

# Generate showcase screenshots
screenshots:
  bash screenshots/generate.sh
