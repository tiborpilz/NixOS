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
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
          domain = "https://grafana.${config.services.reverseProxy.domain}";
        };
      };
    };

    services.prometheus = {
      enable = true;
      port = cfg.prometheusPort;

      exporters = {
        node = {
          enable = true;
          enableCollectors = [ "systemd" ];
        };
      };

      scrapeConfigs = [
        {
          jobName = "node";
          staticConfigs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }
      ];
    };

    modules.services.reverseProxy.proxies.grafana = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
