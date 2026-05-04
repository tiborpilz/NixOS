{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.netmaker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  mosquittoConf = pkgs.writeText "mosquitto.conf" ''
    per_listener_settings false
    listener 8883
    protocol websockets
    allow_anonymous false

    listener 1883
    protocol websockets
    allow_anonymous false

    password_file /mosquitto/password.txt
  '';

  waitSh = pkgs.writeScript "netmaker-mq-wait.sh" ''
    #!/bin/ash

    encrypt_password() {
      echo "netmaker:''${MQ_ADMIN_PASSWORD}" > /mosquitto/password.txt
      mosquitto_passwd -U /mosquitto/password.txt
    }

    main() {
      encrypt_password
      echo "Starting MQ..."
      /docker-entrypoint.sh
      /usr/sbin/mosquitto -c /mosquitto/config/mosquitto.conf
    }

    main "''${@}"
  '';
in
with mylib;
{
  options.modules.services.netmaker = {
    enable = mkBoolOpt false;
    domain = mkOpt types.str "netmaker.tiborpilz.xyz";
    uiDomain = mkOpt types.str "dashboard.tiborpilz.xyz";
    mqDomain = mkOpt types.str "broker.tiborpilz.xyz";
    envFile = mkOption {
      type = types.str;
      description = ''
        Path to env file containing MASTER_KEY and MQ_ADMIN_PASSWORD.
        Typically the path of a sops secret, e.g. config.sops.secrets.netmakerEnv.path.
      '';
    };
  };

  config = mkIf cfg.enable {
    virtualisation.podman.enable = true;

    virtualisation.quadlet =
      let inherit (config.virtualisation.quadlet) pods; in
      {
        pods.netmaker-pod.podConfig = {
          publishPorts = [
            "8081:8081"   # REST API (Caddy proxies to localhost:8081)
            "8082:80"     # UI (Caddy proxies to localhost:8082)
            "1883:1883"   # MQTT plain
            "8883:8883"   # MQTTS — exposed directly, not via Caddy
          ];
        };

        containers.netmaker-server.containerConfig = {
          image = "docker.io/gravitl/netmaker:v0.26.0";
          pod = pods.netmaker-pod.ref;
          volumes = [
            "netmaker-data:/root/data"
          ];
          environments = {
            SERVER_NAME = cfg.domain;
            SERVER_HTTP_HOST = cfg.domain;
            MQ_HOST = cfg.mqDomain;
            BROKER_ENDPOINT = "ssl://${cfg.mqDomain}:8883";
            DISPLAY_KEYS = "on";
            DATABASE = "sqlite";
            NODE_ID = "netmaker-server";
          };
          environmentFiles = [ cfg.envFile ];
        };

        containers.netmaker-ui.containerConfig = {
          image = "docker.io/gravitl/netmaker-ui:v0.26.0";
          pod = pods.netmaker-pod.ref;
          environments = {
            BACKEND_URL = "https://${cfg.domain}";
          };
        };

        containers.netmaker-mq.containerConfig = {
          image = "docker.io/eclipse-mosquitto:2.0.15-openssl";
          pod = pods.netmaker-pod.ref;
          exec = "/mosquitto/config/wait.sh";
          volumes = [
            "${mosquittoConf}:/mosquitto/config/mosquitto.conf:ro"
            "${waitSh}:/mosquitto/config/wait.sh:ro"
            "netmaker-mq-data:/mosquitto/data"
            "netmaker-mq-logs:/mosquitto/log"
          ];
          environmentFiles = [ cfg.envFile ];
        };
      };

    services.caddy = {
      enable = true;
      virtualHosts."${cfg.domain}".extraConfig = "reverse_proxy localhost:8081";
      virtualHosts."${cfg.uiDomain}".extraConfig = "reverse_proxy localhost:8082";
    };

    networking.firewall = {
      allowedTCPPorts = [ 80 443 1883 8883 ];
      allowedUDPPorts = [ 51821 ];
    };
  };
}
