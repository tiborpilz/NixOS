# NixOS & Homemanager configuration

[![built with garnix](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Ftiborpilz%2FNixOS)](https://garnix.io)
[![github actions](https://github.com/tiborpilz/nixos/actions/workflows/build.yml/badge.svg)](https://github.com/tiborpilz/NixOS/actions/workflows/build.yml)


This is my NixOS and Home Manager configuration, which I use on my personal computer. NixOS is a Linux distribution that uses the Nix package manager, and Home Manager is a Nix-based configuration management tool for configuring the user environment. This configuration uses the new Flakes feature in Nix to provide a more modular and flexible way of managing the configuration.

## Layout

This project uses Nix Flakes to manage the configurations. You can read more about Flakes [here](https://wiki.nixos.org/wiki/Flakes).

The Flake has outputs for my Hosts (as NixOS or Nix Darwin) configurations and my Home Manager configurations. The `hosts` directory contains the NixOS and Nix Darwin configurations, and the `home` directory contains the Home Manager configurations.

For reproducibility, I'm using modules defined in the `modules` directory for my hosts and `home/modules` for my Home Manager configuration.

To automatically inject the modules into the configuration, I'm using custom functions defined in `lib`.

## Emacs Config

This repository contains my [literate emacs config](https://github.com/tiborpilz/NixOS/blob/main/home/config/doom/config.org).

## CI

### Garnix

To check whether the configuration is correct and evaluates during build, this project uses [Garnix](https://garnix.io/docs/steps).

### Cachix

This project uses Github Actions to automatically build the configurations (on linux_x86 and darwin_aarch64) and uses [Cachix](https://www.cachix.org/) to store them in a [binary cache](https://app.cachix.org/cache/tiborpilz#pins). This means once my configuration is pushed, I don't have to re-build packages on systems that use it.
