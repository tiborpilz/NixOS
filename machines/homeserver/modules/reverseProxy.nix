{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.reverseProxy;

  reverseProxyOptions = { ... }: {
    options = {
      publicPort = mkOption {
        type = types.int;
        description = "Public port";
        example = 8001;
      };
    };
  };

  mkProxyConfig = port: {
    extraConfig = "reverse_proxy http://localhost:${toString port}";
  };
in
{
  options.reverseProxy = {
    hostname = mkOption {
      default = "";
      type = types.str;
      description = "Host name";
      example = "example.com";
    };
    email = mkOption {
      type = types.str;
      description = "email to use for letsencrypt";
      example = "mail@example.com";
    };
    proxies = mkOption {
      default = { };
      type = types.attrsOf (types.submodule reverseProxyOptions);
      description = "Reverse Proxy";
    };
  };

  config = {
    services.caddy = mkIf (cfg.proxies != { }) (mkMerge [
      {
        virtualHosts = mapAttrs'
          (n: v: nameValuePair "${n}.${cfg.hostname}" (mkProxyConfig v.publicPort))
          cfg.proxies;
        enable = true;
        email = cfg.email;
      }
    ]);
  };
}
