{ config, pkgs, lib, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./disko.nix
  ];

  boot.loader.grub.enable = true;

  # zramSwap.enable = true;

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  sops.age.generateKey = true;

  sops.secrets.netmakerEnv = { };

  services.openssh = {
    enable = true;
    # Port 22 stays here until Netmaker is running and Klaus has a stable mesh IP.
    # At that point: move to 2222 and add NAT forward 22 → Klaus.
    ports = [ 22 ];
  };

  users.users.root.hashedPassword = "$2b$05$jovulKizNS5VrRJ6cyQUjev861XRqF8LDAiZ3pdL5u/h/dIWKWLoy";
  users.users.tibor.isNormalUser = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDEyxiA698x3QgDpUhjGVXo5gtx4oApzYp5RmNleZrsMxGyT89zoGZU1Wi0xlSUF+R0LrTwbOdT+jC4Ym2Ebf497by/w5StxBmiiqHkpq4kGIb8dPP39ez9XaweH1Xi7G3UpNzaJvblDD6jCGWG0zV2CWuHuhRu8O4zRY+63qoh8gKd3aiDRrSJSPv2U1HwIs1ppk3rj7gPIFX/3lmwNo2LA2KKPAvYR1qTmkCnsEEkTzi2zIzztBoHCWSRHtv1374zF/L5EmV+EScL1BEK667kUUDbNAMXVL3juR/Hb/LRwjiO053rhj1NQ+jy2uH1UYwkdg7UM7N4uPKjFtPsC6oPNLCRCH0XpX9vp8U6T4GDz3ypVRzq1zfiRIYl5X+EgqgKw5eHEn/44VWSt/lKv5THcMmb3cMRTfYjUALNhp8XWF2/FsbS02Da7qKGLfUK7kreE+PPJYk3rAWpU+gMaeXU+ILz+sLrD2Lrkkeuq7PocxLAraz3KslO48xXNnM8sc8= tibor@workyMcWorkstation"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCe+KzPWNqzt+9/0EB0x92q0t1dCTHrq4W0Dn/qednCh9Ib8tsm5tbpI3hKJGoOvVfIr2dJe+pJA1mBua15jcShxNPVeB5WwpWMCm5BHB4JZCN5ZeiPvwQ60gB5fEmwq/JCVp1BznOLrC/acKE0elP6r1YD1CcgVd5whH2vibLmP8ahaqXsndUDCRSxoxD/10+8Uk3hLRYGIp8F28oiuumz9nqDhSDk9e/OXve7tASfvuos8t8d6/Hl10LSAPvpeLB6SqSaMn9PfleEMWrlurranB0/JTcT+gZJ6QfnGoKSAMOA3FCgDJ7WfQ/DClggL4xdvdT1SYfUip/sPaDPWP6IidnIOmhnZkpyYuEjUETq4MW5V0jEM5gXOhnvMYzfrM9hAn0ob1WPF/o6uUilmeDIuUb4O9Wj+xpgB1/WuO+4DQJEUXpX0mZTn6lDOiHRm1n+Yg/ZL9yfJsaUGMOPDsKl4QTo2Idaju+2/xtAR1V0LkPduaq8B2bPO5fu4aTaM18= tiborpilz@MacBook-Pro-MBP-L1670"

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

  networking.firewall = {
    enable = true;
    allowPing = true;
    allowedTCPPorts = [ 22 80 443 1883 8883 ];
    allowedUDPPorts = [ 51821 ];
  };
}
