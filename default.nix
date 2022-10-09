{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;

{
  imports =
    [
      inputs.home-manager.nixosModules.home-manager
      inputs.sops-nix.nixosModules.sops
    ] ++ (mapModulesRec' (toString ./modules) import);
}
