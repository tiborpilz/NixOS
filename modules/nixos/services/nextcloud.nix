{ config, lib, ... }:
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
    configDir = mkOption {
      type = types.str;
      default = "/var/lib/nextcloud";
      description = ''
        The config directory of the service.
      '';
    };
    dataDir = mkOption {
      type = types.str;
      default = "";
      description = ''
        The data directory of the service.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.init-nextcloud = stringAfter [ "var" ] ''
      mkdir -p ${cfg.configDir}
      chown -R nextcloud:nextcloud ${cfg.configDir}

      # if dataDir is set, create it
      if [ -n "${cfg.dataDir}" ]; then
        mkdir -p ${cfg.dataDir}
      fi
    '';

    users.users.nextcloud = {
      isSystemUser = true;
      group = "nextcloud";
    };
    users.groups.nextcloud = { };

    containers.nextcloud = {
      bindMounts."${cfg.adminpassFile}" = {
        hostPath = cfg.adminpassFile;
        isReadOnly = true;
      };
      bindMounts."${cfg.dataDir}" = mkIf (cfg.dataDir != "") {
        hostPath = cfg.dataDir;
        isReadOnly = false;
      };

      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.100.10";
      localAddress = "192.168.100.11";
      hostAddress6 = "fd00::10";
      localAddress6 = "fd00::11";

      config = { pkgs, system, ... }@container: {
        system.activationScripts.init-nextcloud-secrets = stringAfter [ "var" ] ''
          mkdir -p /tmp/nextcloud-secrets
          cp ${cfg.adminpassFile} /tmp/nextcloud-secrets/adminpass
          chown -R nextcloud:nextcloud /tmp/nextcloud-secrets
          chown -R nextcloud:nextcloud ${cfg.configDir}

          # if dataDir is set, symlink it to ${cfg.configDir}/data
          if [ -n "${cfg.dataDir}" ]; then
            ln -s ${cfg.dataDir} ${container.config.services.nextcloud.datadir}/data
          fi
        '';

        system.stateVersion = config.system.stateVersion;
        services.nextcloud = {
          enable = true;
          package = pkgs.nextcloud28;
          extraApps = with container.config.services.nextcloud.package.packages.apps; {
            inherit contacts calendar tasks bookmarks;
          };
          extraAppsEnable = true;

          hostName = "nextcloud.${config.modules.services.reverseProxy.hostname}";

          config = {
            adminuser = "admin";
            adminpassFile = "/tmp/nextcloud-secrets/adminpass";
          };
        };
        networking.firewall.allowedTCPPorts = [ 80 ];
        networking.firewall.enable = true;
        # networking.useHostResolvConf = true;
        # networking.defaultGateway.address = "192.168.100.10";
      };
    };

    networking.nat = {
      enable = true;
      internalInterfaces = [ "ve-nextcloud" ];
      externalInterface = "wg0";
      enableIPv6 = true;
    };

    # networking.networkmanager.unmanaged = [ "interface-name:ve-nextcloud" ];

    # networking.firewall.enable = false;
    # networking.firewall.checkReversePath = false;
    # networking.firewall.trustedInterfaces = [ "ve-nextcloud" ];

    modules.services.reverseProxy.proxies.nextcloud = {
      publicPort = 80;
      auth = false;
      targetHost = config.containers.nextcloud.localAddress;
    };
  };
}
