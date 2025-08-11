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

    grafanaPort = mkOption {
      type = types.int;
      default = 9654;
    };
    influxdbPort = mkOption {
      type = types.int;
      default = 8086;
    };
    influxdbDatadir = mkOption {
      type = types.str;
      default = "/data/monitoring/influxdb";
    };
    unifiPollerPort = mkOption {
      type = types.int;
      default = 9130;
    };
    unifiPollerWebDir = mkOption {
      type = types.str;
      default = "/data/monitorin/unifipoller";
    };
    lokiPort = mkOption {
      type = types.int;
      default = 3100;
    };
    lokiDatadir = mkOption {
      type = types.str;
      default = "/data/monitoring/loki";
      system.activationScripts.makeInfluxdbDataDir = stringAfter [ "var" ] ''
        mkdir -p ${cfg.influxdbDatadir}
        mkdir -p ${cfg.unifiPollerWebDir}
      '';
    };


    config = {
      services.grafana = {
        enable = true;
        provision = {
          enable = true;

          datasources.settings.datasources = [{
            name = "Prometheus";
            type = "prometheus";
            access = "proxy";
            url = "http://localhost:${toString config.services.prometheusPort}";
            editable = false;
          }];

          dashboards.settings = {
            apiVersion = 1;

            providers = [{
              name = "default";
              type = "file";
              disableDeletion = false;
              updateIntervalSeconds = 10;
              options = {
                path = "/etc/grafana/dashboards";
                foldersFromFileStructure = true;
              };
            }];
          };
        };

        settings = {
          server = {
            http_addr = "127.0.0.1";
            http_port = cfg.grafanaPort;
            domain = "grafana.${config.services.reverseProxy.hostname}";
            root_url = "https://${config.services.reverseProxy.hostname}";
            serve_from_sub_path = false;
          };
        };
      };

      environment.etc = {
        "grafana/dashboards/overview.json" = {
          source = ./dashboards/overview.json;
          group = "grafana";
          user = "grafana";
        };
      };

      virtualisation.oci-containers.containers.unifipoller = {
        image = "docker.io/golift/unifi-poller:v2.15.3";
        environment = {
          UP_UNIFI_DEFAULT_URL = "https://192.168.1.1:443";
          UP_UNIFI_DEFAULT_ROLE = "URL";
          UP_UNIFI_DEFAULT_USER = "Unifipoller1";
          UP_UNIFI_DEFAULT_PASS = "Unifipoller1";
          UP_UNIFI_DEFAULT_SAVE_SITES = "false";
          UP_UNIFI_DEFAULT_SITE_0 = "default";
          UP_UNIFI_DEFAULT_SAVE_IDS = "false";
          UP_UNIFI_DEFAULT_SAVE_EVENTS = "false";
          UP_UNIFI_DEFAULT_SAVE_ALARMS = "false";
          UP_UNIFI_DEFAULT_SAVE_ANOMALIES = "false";
          UP_UNIFI_DEFAULT_SAVE_DPI = "false";

          UP_GRAFANA_ENABLE = "true";

          UP_INFLUXDB_DISABLE = "false";
          UP_INFLUXDB_URL = "http://192.168.1.51:${toString cfg.influxdbPort}";
          UP_INFLUXDB_DB = "influx";
          UP_INFLUXDB_INTERBVAL = "30s";
        };
        ports = [
          "${toString cfg.unifipollerPort}:9130"
        ];
        volumes = [
          "${cfg.unifiPollerWebDir}:/usr/lib/unpoller/web"
        ];
      };

      # Not entirely declarative, needs initialization of a DB
      virtualisation.oci-containers.containers.monitoring-influxdb = {
        image = "docker.io/influxdb:1.11.8";
        environment = {
          INFLUXDB_HTTP_AUTH_ENABLED = "false";
        };
        volumes = [
          "${cfg.influxdbDatadir}:/var/lib/influxdb2"
        ];
        ports = [
          "${toString cfg.influxdbPort}:8086"
        ];
      };

      services.loki = {
        enable = true;
        configuration = {
          server.http_listen_port = cfg.lokiPort;
          auth_enabled = false;

          ingester = {
            lifecycler = {
              address = "127.0.0.1";
            };
            ring = {
              kvstore = {
                store = "memory";
              };
            };
            replica_factor = 1;
          };
        };
        chunk_idle_period = "1h";
        max_chunk_age = "1h";
        chunk_target_size = 999999;
        chunk_retain_period = "1h";
        max_transfer_retries = 3;
      };

      schema_config = {
        configs = [{
          from = "2022-06-06";
          store = "boltdb-shipper";
          object_store = "filesystem";
          schema = "v11";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }];
      };

      storage_config = {
        boltdb_shipper = {
          active_index_directory = "/var/lib/loki/boltdb-shipper-active";
          cache_location = "/var/lib/loki/boltdb-shipper-cache";
          cache_ttl = "24h";
          shared_store = "filesystem";
        };

        filesystem = {
          directory = "/var/lib/loki/chunks";
        };
      };

      limits_config = {
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
      };

      chunk_store_config = {
        max_look_back_period = "0s";
      };

      table_manager = {
        retention_deletes_enabled = false;
        retention_period = "0s";
      };

      compactor = {
        working_directory = "/var/lib/loki";
        shared_store = "filesystem";
        compactor_ring = {
          kvstore = {
            store = "inmemory";
          };
        };
      };
    };
    # user, group, dataDir, extraFlags, (configFile)

    services.prometheus = {
      enable = true;
      port = cfg.prometheusPort;

      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
        };
      };

      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }
        {
          job_name = "unifipoller";
          static_configs = [{
            targets = [ "127.0.0.1:${toString cfg.unifipollerPort}" ];
          }];
        }
      ];
    };

    services.promtail = {
      enable = true;
      configuration = {
        server = {
          http_listen_port = 3031;
          grpc_listen_port = 0;
        };
        positions = {
          filename = "/tmp/positions.yaml";
        };
        clients = [{
          url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push";
        }];
        scrape_configs = [{
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = "pihole";
            };
          };
          relabel_configs = [{
            source_labels = [ "__journal__systemd_unit" ];
            target_label = "unit";
          }];
        }];
      };
      # extraFlags
    };


    #
    # # Jaeger
    # virtualisation.containers.jaeger = {
    #   image = "jaegertracing/jaeger-all-in-one:1.72.0";
    #   environments = {
    #     COLLECTOR_ZIPKIN_HTTP_PORT = "9411";
    #     JAEGER_AGENT_HOST = "localhost";
    #     JAEGER_AGENT_PORT = "6831";
    #   };
    # };

    modules.services.reverseProxy.proxies.grafana = {
      publicPort = cfg.grafanaPort;
      auth = false;
    };
  };
}
