{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.reverseProxy;

  reverseProxyOptions = { ... }: {
    options = {
      publicPort = mkOption {
        type = types.int;
        description = "Public port";
        example = 8001;
      };
      auth = mkOption {
        type = types.bool;
        description = "Enable authentication";
        example = true;
        default = true;
      };
    };
  };

  basicAuthOptions = { ... }: {
    options = {
      enable = mkOption {
        type = types.bool;
        description = "Enable basic auth";
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

  mkProxyConfig = port: enableauth: username: password: host: {
    serverAliases = [ "http://${host}" ];
    extraConfig = if enableauth then ''
      reverse_proxy http://localhost:${toString port}
      basicauth /* bcrypt {
          ${username} ${password}
      }
    ''
    else "reverse_proxy http://localhost:${toString port}";
  };
in
{
  options.services.reverseProxy = {
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

  config = {
    services.caddy = mkIf (cfg.proxies != { }) (mkMerge [
      {
        virtualHosts = mapAttrs'
          (n: v: nameValuePair "${n}.${cfg.hostname}" (mkProxyConfig v.publicPort (cfg.basicAuth.enable && v.auth) cfg.basicAuth.username cfg.basicAuth.password "${n}.${cfg.hostname}"))
          cfg.proxies;
        enable = true;
        email = cfg.email;
        globalConfig = ''
          auto_https disable_redirects
        '';
      }
    ]);
  };
}
