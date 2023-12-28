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
      settings = {
        server = {
          http_addr = "0.0.0.0";
          http_port = cfg.grafanaPort;
          domain = "https://grafana.${config.modules.services.reverseProxy.hostname}";
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
