# NixOS & Homemanager configuration

[![github actions](https://github.com/tiborpilz/nixos/actions/workflows/build.yml/badge.svg)](https://github.com/tiborpilz/NixOS/actions/workflows/build.yml)


This is my NixOS and Home Manager configuration, which I use on my personal computer. NixOS is a Linux distribution that uses the Nix package manager, and Home Manager is a Nix-based configuration management tool for configuring the user environment. This configuration uses the new Flakes feature in Nix to provide a more modular and flexible way of managing the configuration.

## Layout

This project uses Nix Flakes to manage the configurations. You can read more about Flakes [here](https://wiki.nixos.org/wiki/Flakes).

The Flake has outputs for my Hosts (as NixOS or Nix Darwin) configurations and my Home Manager configurations. The `hosts` directory contains the NixOS and Nix Darwin configurations, and the `home` directory contains the Home Manager configurations.

For reproducibility, I'm using modules defined in the `modules` directory for my hosts and `home/modules` for my Home Manager configuration.

To automatically inject the modules into the configuration, I'm using custom functions defined in `lib`.

## ISOs

`nix build .#isos.<name>`, where each entry under `isos/` is a spec. Two kinds:

- **Installers** (`{ host = "couchthink"; offline = true; }`) — media whose only
  job is to partition and install one of the hosts above. See `lib/installer.nix`.
- **Live systems** (`{ kind = "live"; modules = [ ./system.nix ]; }`) — the image
  *is* the system, so the spec carries its own config instead of pointing at a
  host, and nothing from `modules/` or `hosts/` is merged in. Such a spec is a
  directory (`isos/amnesiac/`) holding both the spec and that config. See
  `lib/live.nix`.

### `amnesiac` — a Tails-like live system

```
nix build .#isos.amnesiac
sudo dd if=result/iso/*.iso of=/dev/sdX bs=4M conv=fsync status=progress
```

Amnesic by construction: the store is a read-only squashfs with a tmpfs
overlay, there is no swap, and journald is volatile. All traffic is forced
through Tor by an nftables kill switch — the nat/output chain redirects TCP to
Tor's TransPort and DNS to its DNSPort, and a filter/output chain with a drop
policy makes anything Tor cannot carry fail closed rather than leak. Boots
straight into Plasma 6 as `amnesia` with Tor Browser, KeePassXC, Thunderbird,
OnionShare, GnuPG, VeraCrypt and `mat2`.

**Persistent storage.** Once, from the running live system:

```
sudo persist-setup
```

That moves the GPT backup header to the real end of the stick, creates a
partition labelled `nixos-persist` in the free space after the ISO, and
LUKS2-formats it. On every later boot `persist-unlock.service` prompts on tty1
before the desktop starts; an empty passphrase keeps the session fully amnesic.
Unlocking bind-mounts `~/.gnupg`, `~/.ssh`, `~/.password-store`, `~/Persistent`
and `/etc/NetworkManager/system-connections` out of it. Anything else you want
to keep goes under `~/Persistent`.

Caveats: rewriting the partition table of a hybrid ISO can break BIOS
(non-UEFI) booting of that stick; the `amnesia` account has no password (run
`passwd` before relying on the screen lock); and if Tor will not bootstrap, the
clock is the usual culprit — `sudo date -s "..."`, since NTP is blocked too.

## Emacs Config

This repository contains my [literate emacs config](https://github.com/tiborpilz/NixOS/blob/main/home/config/doom/config.org).

## CI

This project uses Github Actions to automatically build the configurations (on linux_x86 and darwin_aarch64) and uses [Cachix](https://www.cachix.org/) to store them in a [binary cache](https://app.cachix.org/cache/tiborpilz#pins). This means once my configuration is pushed, I don't have to re-build packages on systems that use it.
