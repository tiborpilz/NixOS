{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.homepage;
  rp = config.modules.services.reverseProxy;
  ms = config.modules.services;

  hostname = rp.hostname;
  localDomain = rp.localDomain;

  url = sub: "https://${sub}.${hostname}";

  # A single homepage service tile. Homepage expects each tile to be a
  # single-key attrset keyed by the display name.
  mkTile = spec: {
    ${spec.name} = {
      href = url spec.sub;
      icon = "${spec.icon}.png";
      description = spec.description;
    } // optionalAttrs cfg.siteMonitors {
      siteMonitor = url spec.sub;
    };
  };

  # Build a homepage group from a list of tile specs, keeping only the ones
  # whose backing service is enabled. Returns a single-key attrset
  # { <group> = [ tiles ]; } so it can be filtered/merged as a unit.
  mkGroup = groupName: specs: {
    ${groupName} = map mkTile (filter (s: s.enable) specs);
  };

  groups = [
    (mkGroup "Media" [
      { enable = ms.media.jellyfin.enable; name = "Jellyfin"; sub = "jellyfin"; icon = "jellyfin"; description = "Movies & TV"; }
      { enable = ms.media.immich.enable; name = "Immich"; sub = "immich"; icon = "immich"; description = "Photos"; }
      { enable = ms.media.audiobookshelf.enable; name = "Audiobookshelf"; sub = "audiobookshelf"; icon = "audiobookshelf"; description = "Audiobooks & podcasts"; }
      { enable = ms.media.komga.enable; name = "Komga"; sub = "komga"; icon = "komga"; description = "Comics & manga"; }
      { enable = ms.media.calibre.enable; name = "Calibre-Web"; sub = "calibre-web"; icon = "calibre-web"; description = "E-books"; }
      { enable = ms.media.pinchflat.enable; name = "Pinchflat"; sub = "pinchflat"; icon = "pinchflat"; description = "YouTube archiver"; }
    ])

    (mkGroup "Downloads" [
      { enable = ms.media.deluge.enable; name = "Deluge"; sub = "deluge"; icon = "deluge"; description = "Torrent client"; }
      { enable = ms.media.sonarr.enable; name = "Sonarr"; sub = "sonarr"; icon = "sonarr"; description = "TV shows"; }
      { enable = ms.media.radarr.enable; name = "Radarr"; sub = "radarr"; icon = "radarr"; description = "Movies"; }
      { enable = ms.media.readarr.enable; name = "Readarr"; sub = "readarr"; icon = "readarr"; description = "Books"; }
      { enable = ms.media.jackett.enable; name = "Jackett"; sub = "jackett"; icon = "jackett"; description = "Indexer proxy"; }
    ])

    (mkGroup "Productivity" [
      { enable = ms.paperless.enable; name = "Paperless"; sub = "paperless"; icon = "paperless-ngx"; description = "Document archive"; }
      { enable = ms.tandoor.enable; name = "Tandoor"; sub = "tandoor"; icon = "tandoor-recipes"; description = "Recipes"; }
      { enable = ms."firefly-iii".enable; name = "Firefly III"; sub = "firefly"; icon = "firefly-iii"; description = "Personal finance"; }
      { enable = config.services.vikunja.enable; name = "Vikunja"; sub = "vikunja"; icon = "vikunja"; description = "Tasks & to-dos"; }
      { enable = ms.linkwarden.enable; name = "Linkwarden"; sub = "linkwarden"; icon = "linkwarden"; description = "Bookmarks & archive"; }
      { enable = ms.linkding.enable; name = "Linkding"; sub = "linkding"; icon = "linkding"; description = "Bookmarks"; }
      { enable = ms.syncthing.enable; name = "Syncthing"; sub = "syncthing"; icon = "syncthing"; description = "File sync"; }
    ])

    (mkGroup "Infrastructure" [
      { enable = ms.forgejo.enable; name = "Forgejo"; sub = "forgejo"; icon = "forgejo"; description = "Git forge"; }
      { enable = ms.woodpecker.enable; name = "Woodpecker CI"; sub = "ci"; icon = "woodpecker-ci"; description = "CI/CD"; }
      { enable = ms.authentik.enable; name = "Authentik"; sub = "auth"; icon = "authentik"; description = "Identity provider"; }
      { enable = rp.proxies ? homeassistant; name = "Home Assistant"; sub = "homeassistant"; icon = "home-assistant"; description = "Home automation"; }
      { enable = ms.frigate.enable; name = "Frigate"; sub = "frigate"; icon = "frigate"; description = "NVR & camera AI"; }
    ])

    (mkGroup "Monitoring" [
      { enable = ms.monitoring.enable; name = "Grafana"; sub = "grafana"; icon = "grafana"; description = "Metrics & dashboards"; }
    ])
  ];

  # Drop groups that ended up with no enabled tiles.
  nonEmptyGroups = filter (g: head (attrValues g) != [ ]) groups;

  port = toString cfg.publicPort;
  machine = config.networking.hostName;

  # Homepage rejects any request whose Host header isn't in this list (a
  # DNS-rebinding guard). Cover every way the dashboard is realistically
  # reached: the reverse-proxy FQDNs, the machine's own hostnames, and
  # localhost — each with and without the listen port, since a direct
  # (non-proxied) hit carries "host:port" while a proxied one carries just
  # the host.
  computedAllowedHosts =
    [ "${cfg.subdomain}.${hostname}" ]
    ++ optional (localDomain != null && localDomain != "") "${cfg.subdomain}.${localDomain}"
    ++ [
      machine "${machine}:${port}"
      "${machine}.local" "${machine}.local:${port}"
      "localhost" "localhost:${port}"
      "127.0.0.1" "127.0.0.1:${port}"
    ]
    ++ cfg.allowedHosts;

  # A literal "*" must be the sole value to mean "allow any host".
  allowedHostsStr =
    if elem "*" cfg.allowedHosts then "*"
    else concatStringsSep "," computedAllowedHosts;
in
{
  options.modules.services.homepage = {
    enable = mkBoolOpt false;

    subdomain = mkOption {
      type = types.str;
      default = "home";
      description = "Subdomain the dashboard is exposed on via the reverse proxy.";
    };

    publicPort = mkOption {
      type = types.int;
      default = 8082;
      description = "Port homepage-dashboard listens on locally.";
    };

    title = mkOption {
      type = types.str;
      default = config.networking.hostName;
      description = "Dashboard title shown in the header and browser tab.";
    };

    allowedHosts = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Extra Host header values homepage-dashboard accepts, on top of the
        reverse-proxy FQDNs and local hostnames computed automatically. Use
        this to reach the dashboard by raw LAN IP, e.g. [ "192.168.1.51:8082" ].
        Set to [ "*" ] to accept any host, which is safe here because the
        reverse proxy and Cloudflare Access already enforce authentication.
      '';
      example = [ "192.168.1.51:8082" ];
    };

    siteMonitors = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Show a live up/down status ping next to each service tile. Monitors
        hit the service's public URL, which resolves to the local Caddy, so
        they reflect real reachability without leaving the host.
      '';
    };
  };

  config = mkIf cfg.enable {
    services.homepage-dashboard = {
      enable = true;
      listenPort = cfg.publicPort;
      allowedHosts = allowedHostsStr;

      settings = {
        title = cfg.title;
        headerStyle = "boxed";
        theme = "dark";
        color = "slate";
        layout = {
          Media = { style = "row"; columns = 4; };
          Downloads = { style = "row"; columns = 4; };
          Productivity = { style = "row"; columns = 4; };
          Infrastructure = { style = "row"; columns = 4; };
          Monitoring = { style = "row"; columns = 4; };
        };
      };

      widgets = [
        {
          resources = {
            label = "klaus";
            cpu = true;
            memory = true;
            disk = "/";
          };
        }
        {
          search = {
            provider = "duckduckgo";
            target = "_blank";
          };
        }
      ];

      services = nonEmptyGroups;
    };

    modules.services.reverseProxy.proxies.${cfg.subdomain} = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
