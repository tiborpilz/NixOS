{ config, lib, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.monitoring;
in
{
  options.modules.services.monitoring = {
    enable = mkBoolOpt false;
    grafanaPort = mkOption {
      type = types.int;
      default = 9654;
    };
    prometheusPort = mkOption {
      type = types.int;
      default = 9655;
    };
  };

  config = mkIf cfg.enable {
    services.grafana = {
      enable = true;
      provision = {
        datasources.settings.datasources = [{
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://localhost:${toString config.services.prometheus.port}";
          editable = false;
        }];
        enable = true;
      };

      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
          domain = "grafana.${config.modules.services.reverseProxy.hostname}";
          root_url = "https://${config.modules.services.reverseProxy.hostname}";
          serve_from_sub_path = false;
        };
      };
    };

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
      ];
    };

    modules.services.reverseProxy.proxies.grafana = {
      publicPort = cfg.grafanaPort;
      auth = false;
    };
  };
}
