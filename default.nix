{ inputs, config, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;

{
  imports =
    [
      inputs.sops-nix.nixosModules.sops
    ] ++ (mapModulesRec' (toString ./modules) import);

    sops.defaultSopsFile = secrets/secrets.yaml;
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    sops.age.keyFile = "/var/lib/sops-nix/key.txt";
    sops.age.generateKey = true;

    # home-manager.useGlobalPkgs = true;
    # home-manager.useUserPackages = true;
    # home-manager.users.tibor = import ./home;
    #
    # home-manager = import ./home-manager; # ) // {
    # home-manager = {

    #   home.username = "tibor";
    #   home.homeDirectory = "/home/tibor";

    #   home.stateVersion = "22.05";

    #   # Let Home Manager install and manage itself.
    #   programs.home-manager.enable = true;
    #   programs.tmux.enable = true;
    #   programs.man.enable = false;
    # };
    #   programs.home-manager.enable = false; # When using NixOS, I don't want to use home-manager standalone.
    # };
}