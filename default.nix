{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;

{
  imports =
    [
      inputs.sops-nix.nixosModules.sops
      inputs.home-manager.nixosModules.home-manager
    ];

  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;

  console.useXkbConfig = true;
}
