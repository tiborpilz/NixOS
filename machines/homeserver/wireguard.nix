{
  # enable NAT
  networking.nat.enable = true;
  networking.nat.externalInterface = "eth0";
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.2/24" ];
      listenPort = 51820;

      privateKeyFile = "/var/lib/wireguard/private.key";

      peers = [
        {
          publicKey = "QzJm9puVez50UZbCUAJYZnqBdW19o1tBU0Q/WXZsbyw=";
          allowedIPs = [ "0.0.0.0/0" ];

          endpoint = "159.69.194.44:51820";

          persistentKeepalive = 25;
        }
      ];
    };
  };
}
