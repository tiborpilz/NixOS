<p align="center">
  <a href="https://github.com/tiborpilz/NixOS/actions/workflows/build.yml"><img src="https://github.com/tiborpilz/nixos/actions/workflows/build.yml/badge.svg" /></a>
</p>
  
# NixOS & Homemanager configuration

This is my NixOS and Home Manager configuration, which I use on my personal computer. NixOS is a Linux distribution that uses the Nix package manager, and Home Manager is a Nix-based configuration management tool for configuring the user environment. This configuration uses the new Flakes feature in Nix to provide a more modular and flexible way of managing the configuration.

## CI

### Garnix

To check whether the configuration is correct and evaluates during build, this project uses [Garnix](https://garnix.io/docs/steps).

### Cachix

This project uses Github Actions to automatically build the configurations and use [Cachix](https://www.cachix.org/) to store them in a [binary cache](https://app.cachix.org/cache/tiborpilz#pins). This means once my configuration is pushed, I don't have to re-build packages on systems that use it.
