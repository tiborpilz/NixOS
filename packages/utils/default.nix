{ inputs, pkgs, lib, ... }:
  lib.my.mapModulesRec (toString ./.) (package: (import package) { inherit inputs pkgs lib; })
