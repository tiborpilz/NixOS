{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.mosquitto;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.mosquitto = {
    enable = mkBoolOpt false;
    port = mkOption {
      type = types.port;
      default = 1883;
      description = "MQTT listener port";
    };
    users = mkOption {
      type = types.attrsOf types.path;
      default = { };
      description = "MQTT username to password file path.";
    };
    acl = mkOption {
      type = types.listOf types.str;
      default = [ "topic readwrite #" ];
    };
    # When set, expose a second listener speaking MQTT over WebSockets on
    # this port (browser clients cannot use plain TCP 1883). Also adds a
    # wss:// bridge ("mqtt-ws" proxy) through the reverse proxy.
    websocketsPort = mkOption {
      type = types.nullOr types.port;
      default = null;
    };
    webUi = {
      enable = mkBoolOpt false;
      publicPort = mkOption {
        type = types.int;
        default = 8084;
      };
      image = mkOption {
        type = types.str;
        default = "docker.io/emqx/mqttx-web:latest";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.mosquitto = {
      enable = true;
      listeners =
        let
          users = mapAttrs (_: passwordFile: { inherit passwordFile; }) cfg.users;
        in
        [
          {
            inherit (cfg) port acl;
            settings = {
             allow_anonymous = false;
            };
            inherit users;
          }
        ]
        ++ optionals (cfg.websocketsPort != null) [
          {
            port = cfg.websocketsPort;
            inherit (cfg) acl;
            inherit users;
            settings = {
              allow_anonymous = false;
              protocol = "websockets";
            };
          }
        ];
    };

    modules.services.reverseProxy.proxies = mkIf (cfg.websocketsPort != null) {
      "mqtt-ws" = {
        publicPort = cfg.websocketsPort;
        auth = false;
      };
    };

    # MQTTX Web: browser-based MQTT client/dashboard. A pure client-side
    # app -- it keeps connections/settings in browser localStorage and needs
    # no volumes. The browser reaches the broker over WebSockets (see
    # websocketsPort above): ws://<host>:<websocketsPort> on the LAN, or
    # wss://mqtt-ws.<hostname> through the tunnel.
    virtualisation.oci-containers.containers.mqttx-web = mkIf cfg.webUi.enable {
      image = cfg.webUi.image;
      ports = [ "${toString cfg.webUi.publicPort}:80" ];
    };

    # Gated by Cloudflare Access rather than the instance-wide basic auth,
    # like the other LAN services.
    modules.services.reverseProxy.proxies = mkIf cfg.webUi.enable {
      mqttx = {
        publicPort = cfg.webUi.publicPort;
        auth = false;
      };
    };
  };
}
