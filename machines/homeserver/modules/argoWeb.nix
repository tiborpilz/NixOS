{ config, lib, pkgs, ... }:
with lib;

let
  # cfg = config.services.argoWeb;
  etcDir = "argoWeb";

  argoWebConfig = {
    tunnel = "564f55fe-2f10-42a3-bc77-23b2dae810e2";
    credentials-file = "/etc/${etcDir}/uuid.json";
    loglevel = "warn";
  };

  argoUuid = {
    "AccountTag" = "65ecf0caf1b5e94e37b25f84e11e0a90";
    "TunnelSecret" = "6f9Xya72bs5FrCG03Byw5ARQDVRAyYEiBP6W0llRUUY=";
    "TunnelID" = "564f55fe-2f10-42a3-bc77-23b2dae810e2";
  };

in
{
  # options.sercvices.argoWeb = {
  #   enable = mkEnableOption "Cloudflare Argo Tunnel";
  # };

  # config = mkIf cfg.enable {

  environment.etc = {
    "${etcDir}/uuid.json" = {
      text = generators.toJSON {} argoUuid;
      mode = "0600";
    };
    "${etcDir}/argoWeb.yaml" = {
      text = generators.toYAML {} argoWebConfig;
      mode = "0600";
    };
  };

  users.users.argoWeb = {
    home = "/var/lib/argoWeb";
    createHome = true;
    isSystemUser = true;
    group = "argoWeb";
    # extraGroups = ["caddyProxy"];
  };

  systemd.services.argoWeb = {
    description = "Cloudflare Argo Tunnel";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ]; # systemd-networkd-wait-online.service
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared --config /etc/${etcDir}/argoWeb.yml --no-autoupdate tunnel run";
      Type = "simple";
      User = "root";
      Group = "root";
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
}
