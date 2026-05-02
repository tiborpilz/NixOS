{ config, pkgs, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./disko.nix
  ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };
  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  sops.age.generateKey = true;

  sops.secrets.netmakerEnv = { };

  services.openssh = {
    enable = true;
    ports = [ 2222 ];
  };

  users.users.tibor.isNormalUser = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDEyxiA698x3QgDpUhjGVXo5gtx4oApzYp5RmNleZrsMxGyT89zoGZU1Wi0xlSUF+R0LrTwbOdT+jC4Ym2Ebf497by/w5StxBmiiqHkpq4kGIb8dPP39ez9XaweH1Xi7G3UpNzaJvblDD6jCGWG0zV2CWuHuhRu8O4zRY+63qoh8gKd3aiDRrSJSPv2U1HwIs1ppk3rj7gPIFX/3lmwNo2LA2KKPAvYR1qTmkCnsEEkTzi2zIzztBoHCWSRHtv1374zF/L5EmV+EScL1BEK667kUUDbNAMXVL3juR/Hb/LRwjiO053rhj1NQ+jy2uH1UYwkdg7UM7N4uPKjFtPsC6oPNLCRCH0XpX9vp8U6T4GDz3ypVRzq1zfiRIYl5X+EgqgKw5eHEn/44VWSt/lKv5THcMmb3cMRTfYjUALNhp8XWF2/FsbS02Da7qKGLfUK7kreE+PPJYk3rAWpU+gMaeXU+ILz+sLrD2Lrkkeuq7PocxLAraz3KslO48xXNnM8sc8= tibor@workyMcWorkstation"
  ];

  programs.zsh.enable = true;
  home.enable = false;

  environment.systemPackages = with pkgs; [
    tmux
    vim
  ];

  system.stateVersion = "22.05";

  modules.services.netmaker = {
    enable = true;
    domain = "netmaker.tiborpilz.xyz";
    uiDomain = "dashboard.tiborpilz.xyz";
    mqDomain = "broker.tiborpilz.xyz";
    envFile = config.sops.secrets.netmakerEnv.path;
  };

  # Move edge SSH off port 22 so port 22 can be forwarded to Klaus for Forgejo
  networking.nat = {
    enable = true;
    enableIPv6 = false;
    externalInterface = "enp1s0";
    forwardPorts = [
      { sourcePort = 22; destination = "10.220.0.2:22"; proto = "tcp"; }
    ];
  };

  networking.firewall = {
    enable = true;
    allowPing = true;
    allowedTCPPorts = [ 22 80 443 2222 1883 8883 ];
    allowedUDPPorts = [ 51821 ];
  };
}
