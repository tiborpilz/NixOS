{ config, lib, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.monitoring;
in
{
  options.modules.services.monitoring = {
    enable = mkBoolOpt false;

    prometheusPort = mkOption {
      type = types.int;
      default = 9655;
    };
    grafanaPort    = mkOption {
      type = types.int;
      default = 9654;
    };
    influxdbPort   = mkOption {
        type = types.int;
default = 8086;
    };
    influxdbDatadir = mkOption {
        type = types.str;
        default = "/var/lib/influxdb";
    };
    unifiWebPort = mkOption {
        type = types.int;
        default = 9226;
    };
    unifiPollerPort = mkOption {
        type = types.int;
        default = 9130;
    };
    unifiPollerWebDir = mkOption {
        type = types.str;
        default = "/data/monitoring/unifipoller";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeMonitoringDirs = ''
      mkdir -p ${cfg.influxdbDatadir}
      mkdir -p ${cfg.unifiPollerWebDir}
    '';

    services.grafana = {
      enable = true;
      provision = {
        enable = true;
        datasources.settings.datasources = [{
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://127.0.0.1:${toString cfg.prometheusPort}";
          editable = false;
        }];
        dashboards.settings = {
          apiVersion = 1;
          providers = [{
            name = "default";
            type = "file";
            disableDeletion = false;
            updateIntervalSeconds = 10;
            options = { path = "/etc/grafana/dashboards"; foldersFromFileStructure = true; };
          }];
        };
      };
      settings.server = {
        http_addr = "127.0.0.1";
        http_port = cfg.grafanaPort;
        domain    = "grafana.${config.modules.services.reverseProxy.hostname}";
        root_url  = "https://grafana.${config.modules.services.reverseProxy.hostname}";
        serve_from_sub_path = false;
      };
    };

    environment.etc."grafana/dashboards/overview.json" = {
      source = ./dashboards/overview.json;
      user = "grafana"; group = "grafana";
    };

    virtualisation.oci-containers.containers.unifipoller = {
      image = "docker.io/golift/unifi-poller:v2.15.3";
      environment = {
        UP_UNIFI_DEFAULT_URL = "https://192.168.1.1:443";
        UP_UNIFI_DEFAULT_ROLE = "URL";
        UP_UNIFI_DEFAULT_USER = "Unifipoller1";
        UP_UNIFI_DEFAULT_PASS = "Unifipoller1";
        UP_UNIFI_DEFAULT_SITE_0 = "default";

        UP_UNIFI_DEFAULT_SAVE_SITES = "true";
        UP_UNIFI_DEFAULT_SAVE_IDS = "false"; # These four only work with loki
        UP_UNIFI_DEFAULT_SAVE_EVENTS = "false";
        UP_UNIFI_DEFAULT_SAVE_ALARMS = "false";
        UP_UNIFI_DEFAULT_SAVE_ANOMALIES = "false";
        UP_UNIFI_DEFAULT_SAVE_DPI = "true";

        UP_PROMETHEUS_DISABLE = "false";

        # TODO InfluxDB is not working properly
        UP_INFLUXDB_DISABLE = "true";
        UP_INFLUXDB_URL = "http://192.168.1.51:${toString cfg.influxdbPort}";
        UP_INFLUXDB_DB = "unpoller";
        UP_INFLUXDB_USER = "unifipoller";
        UP_INFLUXDB_PASS = "unifipoller";
        UP_INFLUXDB_ORG = "unpoller";
        UP_INFLUXDB_BUCKET = "unpoller";
        UP_INFLUXDB_INTERVAL = "20s";

        UP_POLLER_DEBUG = "true";
        UP_POLLER_QUIET = "false";
        UP_WEBSERVER_ENABLE = "true";
        UP_WEBSERVER_PORT = "9226";
      };
      ports = [
        "${toString cfg.unifiWebPort}:9226"
        "${toString cfg.unifiPollerPort}:9130"
      ];
      volumes = [ "${cfg.unifiPollerWebDir}:/usr/local/lib/unpoller/web" ];
    };

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) network pods;
      in
      {
        containers = {
          monitoring-influxdb.containerConfig = {
            image = "docker.io/influxdb:1.8.7";
            volumes = [ "monitoring-influxdb:/var/lib/influxdb" ];
            pod = pods.monitoring-pod.ref;

            environments = {
              INFLUXDB_DB = "unpoller";
              INFLUXDB_HTTP_AUTH_ENABLED = "false";

              DOCKER_INFLUXDB_INIT_MODE = "setup";
              DOCKER_INFLUXDB_INIT_USERNAME = "unifipoller";
              DOCKER_INFLUXDB_INIT_PASSWORD = "unifipoller";
              DOCKER_INFLUXDB_INIT_ORG = "unpoller";
              DOCKER_INFLUXDB_INIT_BUCKET = "unpoller";
              DOCKER_INFLUXDB_INIT_ADMIN_TOKEN = "unpoller";
            };
          };
        };

        pods.monitoring-pod.podConfig = {
          publishPorts = [ "${toString cfg.influxdbPort}:8086" ];
        };
      };

    services.prometheus = {
      enable = true;
      port = cfg.prometheusPort;
      exporters.node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
      };

      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [{ targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ]; }];
        }
        {
          job_name = "unifipoller";
          #static_configs = [{ targets = [ "127.0.0.1:${toString cfg.unifiPollerPort}" ]; }];
          static_configs = [{ targets = [ "127.0.0.1:{toString config.services.prometheus.exporters.node.port}" ]; }];
        }
      ];
    };

    modules.services.reverseProxy.proxies.grafana = {
      publicPort = cfg.grafanaPort;
      auth = false;
    };
  };
}

