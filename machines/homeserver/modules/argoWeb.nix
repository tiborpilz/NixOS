{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.argoWeb;
  etcDir = "argo";

  argoWebConfig = {
    tunnel = "homeservertunnel";
    credentials-file = "/etc/${etcDir}/uuid.json";
    loglevel = "warn";

    ingress = [
      {
        hostname = "tiborpilz.xyz";
        service = "http://localhost:8285";
      }
      {
        service = "http_status:404";
      }
    ];
  };

  argoUuid = {
    "AccountTag" = "65ecf0caf1b5e94e37b25f84e11e0a90";
  };

in
{
  options.sercvices.argoWeb = {
    enable = mkEnableOption "Cloudflare Argo Tunnel";
  };

  config = mkIf cfg.enable {

    environment.etc."${etcDir}"."uuid.json" = {
      text = toJSON argoUuid;
      mode = "0600";
    };

    environment.etc."${etcDir}"."argoWeb.yml" = {
      text = toYAML argoWebConfig;
      mode = "0600";
    };

    users.users.argoWeb = {
      home = "/var/lib/argoWeb";
      createHome = true;
      isSystemUser = true;
      group = "argoWeb";
    };

    groups.caddyProxy.members = [ "argoWeb" ];

    systemd.services.argoWeb = {
      description = "Cloudflare Argo Tunnel";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ]; # systemd-networkd-wait-online.service
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/cloudflared --config /etc/${etcDir}/argoWeb.yml --no-autoupdate tunnel run";
        Type = "simple";
        User = "argoWeb";
        Group = "argoWeb";
        Restart = "on-failure";
        RestartSec = "5s";
        NoNewPrivileges = true;
        LimitNPROC = 512;
        LimitNOFILE = 1048576;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectHome = true;
        ProtectSystem = "full";
        ReadWriteDirectories = /var/lib/argoWeb;
      };
    };
  };
}
