{ config, lib, pkgs, ... }:

let
  cfg = config.modules.services.monitoring.netdata;
in
{
  options.modules.services.monitoring.netdata = {
    enable = lib.mkEnableOption "Enahble Netdata";
  };

  config = lib.mkIf cfg.enable {
    services.netdata = {
      package = pkgs.netdata.override {
        withCloudUi = true;
      };
      enable = true;
      config = {
        global = {
          "memory mode" = "ram";
          "debug log" = "none";
          "access log" = "none";
          "error log" = "syslog";
        };
      };
    };
  };

  # modules.services.reverseProxy.proxies.netdata.publicPort = 19999;
}

