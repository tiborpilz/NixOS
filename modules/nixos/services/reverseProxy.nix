{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.reverseProxy;

  reverseProxyOptions = { ... }: {
    options = {
      publicPort = mkOption {
        type = types.int;
        description = "Public port";
        example = 8001;
      };
      auth = mkOption {
        type = types.bool;
        description = "Enable authentication (per Service)";
        example = true;
        default = true;
      };
      targetHost = mkOption {
        type = types.str;
        description = "Target host (in most cases, localhost)";
        example = "localhost";
        default = "localhost";
      };
    };
  };

  basicAuthOptions = { ... }: {
    options = {
      enable = mkOption {
        type = types.bool;
        description = "Enable basic auth (globally)";
        example = true;
        default = false;
      };
      username = mkOption {
        type = types.string;
        description = "Username";
        example = "admin";
      };
      password = mkOption {
        type = types.string;
        description = "Hashed Password";
        example = "JDJhJDE0JGFXdjZMeTVsYnZueDZpckpQazFkSE9zN283WnZUQmc4NmQydi5rV04wYmdOZ3F0cE4zb3NP";
      };
    };
  };

  mkProxyConfig = { port, enableauth, username, password, host, targetHost, ... }: {
    serverAliases = [ "http://${host}" ];
    extraConfig =
      if enableauth then ''
        reverse_proxy http://${targetHost}:${toString port}
        basicauth /* bcrypt {
            ${username} ${password}
        }
      ''
      else "reverse_proxy http://${targetHost}:${toString port}";
  };
in
{
  options.modules.services.reverseProxy = {
    enable = mkBoolOpt false;
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
    basicAuth = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = "Enable basic auth";
        example = true;
      };
      username = mkOption {
        type = types.str;
        description = "Username";
        example = "admin";
      };
      password = mkOption {
        type = types.str;
        description = "Hashed Password";
        example = "JDJhJDE0JGFXdjZMeTVsYnZueDZpckpQazFkSE9zN283WnZUQmc4NmQydi5rV04wYmdOZ3F0cE4zb3NP";
      };
    };
  };

  config = mkIf cfg.enable {
    services.caddy = mkIf (cfg.proxies != { }) (mkMerge [
      {
        virtualHosts = mapAttrs'
          (n: v: nameValuePair "${n}.${cfg.hostname}" (mkProxyConfig {
            port = v.publicPort;
            enableauth = (cfg.basicAuth.enable && v.auth);
            username = cfg.basicAuth.username;
            password = cfg.basicAuth.password;
            host = "${n}.${cfg.hostname}";
            targetHost = v.targetHost;
          })) cfg.proxies // {
            health = {
              serverAliases = [ "http://health.${cfg.hostname}" ];
              extraConfig = "respond \"OK\"";
            };
          };
        enable = true;
        email = cfg.email;
        globalConfig = ''
        '';
      }
    ]);
  };
}
