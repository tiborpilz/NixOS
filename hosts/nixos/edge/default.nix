{ pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.cleanTmpDir = true;

  boot.kernelModules = [
    "iptable_nat"
    "iptable_filter"
    "xt_nat"
    "ipt_dnat"
  ];

  zramSwap.enable = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDEyxiA698x3QgDpUhjGVXo5gtx4oApzYp5RmNleZrsMxGyT89zoGZU1Wi0xlSUF+R0LrTwbOdT+jC4Ym2Ebf497by/w5StxBmiiqHkpq4kGIb8dPP39ez9XaweH1Xi7G3UpNzaJvblDD6jCGWG0zV2CWuHuhRu8O4zRY+63qoh8gKd3aiDRrSJSPv2U1HwIs1ppk3rj7gPIFX/3lmwNo2LA2KKPAvYR1qTmkCnsEEkTzi2zIzztBoHCWSRHtv1374zF/L5EmV+EScL1BEK667kUUDbNAMXVL3juR/Hb/LRwjiO053rhj1NQ+jy2uH1UYwkdg7UM7N4uPKjFtPsC6oPNLCRCH0XpX9vp8U6T4GDz3ypVRzq1zfiRIYl5X+EgqgKw5eHEn/44VWSt/lKv5THcMmb3cMRTfYjUALNhp8XWF2/FsbS02Da7qKGLfUK7kreE+PPJYk3rAWpU+gMaeXU+ILz+sLrD2Lrkkeuq7PocxLAraz3KslO48xXNnM8sc8= tibor@workyMcWorkstation"
  ];

  programs.zsh.enable = true;

  users.users.tibor = {
    uid = 1000;
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    password = "password";
  };

  environment.systemPackages = with pkgs; [
    tmux
    vim
    wireguard-tools
  ];
  system.stateVersion = "22.05";

  #Enable NAT
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    externalInterface = "enp1s0";
    internalInterfaces = [ "wg0" ];
    forwardPorts = [
      { sourcePort = 80; destination = "10.0.0.2:80"; proto = "tcp"; }
      { sourcePort = 443; destination = "10.0.0.2:443"; proto = "tcp"; }
    ];
    internalIPs = [ "10.0.0.1/24" ];
  };
  # Open ports in the firewall
  networking.firewall = {
    enable = true;
    allowPing = true;
    logRefusedConnections = true;
    rejectPackets = false;
    allowedTCPPorts = [ 53 80 ];
    allowedUDPPorts = [ 53 51820 ];
    trustedInterfaces = [ "wg0" ];
  };
  networking.wg-quick.interfaces = {
    wg0 = {
      # Determines the IP/IPv6 address and subnet of the client's end of the tunnel interface
      address = [ "10.0.0.1/24" "fdc9:281f:04d7:9ee9::1/64" ];
      listenPort = 51820;
      privateKeyFile = "/var/lib/wireguard/private.key";

      # This allows the wireguard server to route the traffic to the internet and hence be like a VPN
      postUp = ''
        ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.0.0.1/24 -o enp1s0 -j MASQUERADE
        ${pkgs.iptables}/bin/ip6tables -A FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o enp1s0 -j MASQUERADE
      '';

      # Undo the above
      preDown = ''
        ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.0.0.1/24 -o enp1s0 -j MASQUERADE
        ${pkgs.iptables}/bin/ip6tables -D FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o enp1s0 -j MASQUERADE
      '';

      peers = [
        {
          # peer0
          publicKey = "mydrVBi96wSZXOYt5a7TcWcNGs2AtDyd8BJHskJLsSU=";
          allowedIPs = [ "10.0.0.2/32" "fdc9:281f:04d7:9ee9::2/128" ];
        }
        # More peers can be added here.
      ];
    };
  };
  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      interface=wg0
    '';
  };
}
