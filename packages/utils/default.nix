{ inputs, pkgs, lib, ... }:
  lib.my.mapModules (toString ./.) (package: (import package) { inherit inputs pkgs lib; })
