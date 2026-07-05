# NixOS & Homemanager configuration

[![github actions](https://github.com/tiborpilz/nixos/actions/workflows/build.yml/badge.svg)](https://github.com/tiborpilz/NixOS/actions/workflows/build.yml)


This is my NixOS and Home Manager configuration, which I use on my personal computer. NixOS is a Linux distribution that uses the Nix package manager, and Home Manager is a Nix-based configuration management tool for configuring the user environment. This configuration uses the new Flakes feature in Nix to provide a more modular and flexible way of managing the configuration.

## Layout

This project uses Nix Flakes to manage the configurations. You can read more about Flakes [here](https://wiki.nixos.org/wiki/Flakes).

The Flake has outputs for my Hosts (as NixOS or Nix Darwin) configurations and my Home Manager configurations. The `hosts` directory contains the NixOS and Nix Darwin configurations, and the `home` directory contains the Home Manager configurations.

For reproducibility, I'm using modules defined in the `modules` directory for my hosts and `home/modules` for my Home Manager configuration.

To automatically inject the modules into the configuration, I'm using custom functions defined in `lib`.

## WSL (Windows Subsystem for Linux)

This config can run as a full NixOS system inside WSL2 via the
[`wsl`](hosts/nixos/wsl/default.nix) host (built on
[NixOS-WSL](https://github.com/nix-community/NixOS-WSL)). Build a rootfs
tarball on any machine with Nix and import it into Windows:

```sh
nix build .#nixosConfigurations.wsl.config.system.build.tarballBuilder
sudo ./result/bin/nixos-wsl-tarball-builder   # -> nixos.wsl
# then, on Windows:
wsl --install --from-file nixos.wsl
```

Afterwards, rebuild from inside WSL with
`sudo nixos-rebuild switch --flake .#wsl`.

## GitHub Pages

A public overview page for this repository is published with GitHub Pages at [tiborpilz.github.io/NixOS](https://tiborpilz.github.io/NixOS/).

## Emacs Config

This repository contains my [literate emacs config](https://github.com/tiborpilz/NixOS/blob/main/home/config/doom/config.org).

## CI

This project uses Github Actions to automatically build the configurations (on linux_x86 and darwin_aarch64) and uses [Cachix](https://www.cachix.org/) to store them in a [binary cache](https://app.cachix.org/cache/tiborpilz#pins). This means once my configuration is pushed, I don't have to re-build packages on systems that use it.
