{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.nextcloud;
in
{
  options.modules.services.nextcloud = {
    enable = mkEnableOption "Nextcloud";
    adminpassFile = mkOption {
      type = types.str;
      default = "";
      description = ''
        Path to the file containing the admin password.
      '';
    };
    publicPort = mkOption {
      type = types.int;
      default = 8510;
      description = ''
        The port on which the service will be exposed.
      '';
    };
    home = mkOption {
      type = types.str;
      default = "/var/lib/nextcloud";
      description = ''
        The home directory of the service.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.init-nextcloud = stringAfter [ "var" ] ''
      mkdir -p ${cfg.home}
      chown -R nextcloud:nextcloud ${cfg.home}
    '';

    users.users.nextcloud = {
      isSystemUser = true;
      home = cfg.home;
      group = "nextcloud";
    };
    users.groups.nextcloud = {};

    containers.nextcloud = {
      bindMounts."${cfg.home}" = {
        hostPath = cfg.home;
        isReadOnly = false;
      };

      bindMounts."${cfg.adminpassFile}" = {
        hostPath = cfg.adminpassFile;
        isReadOnly = true;
      };

      # forwardPorts = [{
      #   containerPort = 80;
      #   hostPort = cfg.publicPort;
      #   protocol = "tcp";
      # }];

      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.100.10";
      localAddress = "192.168.100.11";
      hostAddress6 = "fd00::10";
      localAddress6 = "fd00::11";

      config = { pkgs, ... }: {
        system.stateVersion = config.system.stateVersion;

        services.nextcloud = {
          enable = true;
          hostName = "nextcloud.${config.modules.services.reverseProxy.hostname}";
          package = pkgs.nextcloud27;
          home = cfg.home;

          config = {
            adminuser = "admin";
            adminpassFile = cfg.adminpassFile;
          };
        };

        networking.firewall.allowedTCPPorts = [ 80 ];
        networking.firewall.enable = true;
        networking.firewall.extraCommands = ''
          iptables -F INPUT
          iptables -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
          iptables -A INPUT -s 192.168.100.10 -p tcp --dport 80 -j ACCEPT
          iptables -A INPUT -j DROP
        '';
        networking.useHostResolvConf = true;

        networking.defaultGateway.address = "192.168.100.10";
      };
    };

    networking.nat = {
      enable = true;
      internalInterfaces = [ "ve-nextcloud" ];
      externalInterface = "wlp4s0";
      enableIPv6 = true;
    };

    networking.networkmanager.unmanaged = [ "interface-name:ve-nextcloud" ];

    networking.firewall.enable = true;
    networking.firewall.checkReversePath = false;
    networking.firewall.trustedInterfaces = [ "ve-nextcloud" ];
    networking.firewall.extraCommands ="iptables -t nat -A POSTROUTING -o wg0 -j MASQUERADE";

    modules.services.reverseProxy.proxies.nextcloud = {
      publicPort = 80;
      auth = false;
      targetHost = config.containers.nextcloud.localAddress;
    };
  };
}
