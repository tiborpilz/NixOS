{ pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    # ./wireguard.nix
  ];

  boot.cleanTmpDir = true;
  zramSwap.enable = true;
  networking.hostName = "nix-edge";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDEyxiA698x3QgDpUhjGVXo5gtx4oApzYp5RmNleZrsMxGyT89zoGZU1Wi0xlSUF+R0LrTwbOdT+jC4Ym2Ebf497by/w5StxBmiiqHkpq4kGIb8dPP39ez9XaweH1Xi7G3UpNzaJvblDD6jCGWG0zV2CWuHuhRu8O4zRY+63qoh8gKd3aiDRrSJSPv2U1HwIs1ppk3rj7gPIFX/3lmwNo2LA2KKPAvYR1qTmkCnsEEkTzi2zIzztBoHCWSRHtv1374zF/L5EmV+EScL1BEK667kUUDbNAMXVL3juR/Hb/LRwjiO053rhj1NQ+jy2uH1UYwkdg7UM7N4uPKjFtPsC6oPNLCRCH0XpX9vp8U6T4GDz3ypVRzq1zfiRIYl5X+EgqgKw5eHEn/44VWSt/lKv5THcMmb3cMRTfYjUALNhp8XWF2/FsbS02Da7qKGLfUK7kreE+PPJYk3rAWpU+gMaeXU+ILz+sLrD2Lrkkeuq7PocxLAraz3KslO48xXNnM8sc8= tibor@workyMcWorkstation"
  ];

  environment.systemPackages = with pkgs; [
    tmux
    vim
    wireguard-tools
  ];
  system.stateVersion = "22.05";
  nixpkgs.config.allowUnfree = true;


  # Enable NAT
  # networking.nat = {
  #   enable = true;
  #   enableIPv6 = true;
  #   externalInterface = "eth0";
  #   internalInterfaces = [ "wg0" ];
  # };
  # Open ports in the firewall
  networking.firewall.enable = false;
  # networking.firewall = {
  #   allowedTCPPorts = [ 53 80 ];
  #   allowedUDPPorts = [ 53 51820 ];
  # };
  networking.wg-quick.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP/IPv6 address and subnet of the client's end of the tunnel interface
      address = [ "10.0.0.1/24" "fdc9:281f:04d7:9ee9::1/64" ];
      # The port that WireGuard listens to - recommended that this be changed from default
      listenPort = 51820;
      # Path to the server's private key
      privateKeyFile = "/var/lib/wireguard/private.key";

      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      # postUp = ''
      #   ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
      #   ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.0.0.1/24 -o eth0 -j MASQUERADE
      #   ${pkgs.iptables}/bin/ip6tables -A FORWARD -i wg0 -j ACCEPT
      #   ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o eth0 -j MASQUERADE
      # '';

      # Undo the above
      # preDown = ''
      #   ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
      #   ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.0.0.1/24 -o eth0 -j MASQUERADE
      #   ${pkgs.iptables}/bin/ip6tables -D FORWARD -i wg0 -j ACCEPT
      #   ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o eth0 -j MASQUERADE
      # '';

      peers = [
        { # peer0
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

  services.caddy = {
    enable = true;
    virtualHosts."*.tiborpilz.xyz".extraConfig = ''
      reverse_proxy http://10.0.0.2:80
    '';
  };
}
