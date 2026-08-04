{ config, lib, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.media.immich-public-proxy;
  publicPort = 3080;
  # Host-published port of the immich pod, see ./immich.nix
  immichPort = 2284;
in
{
  options.modules.services.media.immich-public-proxy = {
    enable = mkBoolOpt false;
    version = mkOption {
      type = types.str;
      default = "3.2.0";
    };
    hostname = mkOption {
      type = types.str;
      description = ''
        Public apex domain serving the proxy. This gets its own Cloudflare
        Tunnel ingress rule rather than a `reverseProxy.proxies` entry, since
        those are all bound to `<name>.<reverseProxy.hostname>`.

        Treat the whole zone as public-by-design: nothing else may be served
        from it.
      '';
      example = "tibor.pics";
    };
    shares = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Vanity paths under `hostname`, mapping path to the share target the
        proxy serves -- either "share/<key>" or "s/<slug>". So
        `{ hochzeit = "share/aPgy..."; }` makes https://<hostname>/hochzeit
        redirect to /share/aPgy... .

        Values are placed verbatim into a Caddy `redir`, so they may use
        `{env.VAR}` to pull the key from `services.caddy.environmentFile`
        rather than committing it -- e.g. "share/{env.IPP_SHARE_HOCHZEIT}".

        Note that a vanity path is guessable by design, so it defeats the
        entropy of the key it points at; keeping the key out of git limits
        exposure to people who guess the path, not to anyone crawling the repo.
        Use a shared-link password if the contents actually need gating.
      '';
      example = { hochzeit = "share/{env.IPP_SHARE_HOCHZEIT}"; };
    };
    settings = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Options placed under the `ipp` key of the proxy's config, passed as
        inline JSON in the CONFIG env var rather than a mounted file.

        Partial configs are safe: the proxy reads each option with its own
        fallback instead of deep-merging a file, so anything omitted here keeps
        its upstream default.
      '';
      example = {
        allowDownload = 1;
        gallery.groupByDate = "day";
      };
    };
  };

  config = mkIf cfg.enable {
    # Deliberately its own container rather than a member of immich-pod: this is
    # the only internet-reachable piece, so it must not share a network
    # namespace with the immich database.
    virtualisation.quadlet.containers.immich-public-proxy.containerConfig = {
      image = "docker.io/alangrainger/immich-public-proxy:${cfg.version}";
      publishPorts = [ "${toString publicPort}:3000" ];
      environments = {
        IMMICH_URL = "http://host.containers.internal:${toString immichPort}";
        PUBLIC_BASE_URL = "https://${cfg.hostname}";
      } // optionalAttrs (cfg.settings != { }) {
        CONFIG = builtins.toJSON { ipp = cfg.settings; };
      };
    };

    services.cloudflared.tunnels.${config.modules.services.reverseProxy.tunnelId}.ingress = {
      ${cfg.hostname} = "http://localhost:80";
    };

    # http:// scheme only -- the tunnel terminates TLS at Cloudflare and speaks
    # plain HTTP to localhost:80, so Caddy must not attempt ACME for this name.
    services.caddy.virtualHosts."http://${cfg.hostname}".extraConfig = ''
      ${concatStringsSep "\n" (mapAttrsToList (path: target:
        "redir /${path} /${target} 302"
      ) cfg.shares)}

      reverse_proxy http://localhost:${toString publicPort}
    '';
  };
}
