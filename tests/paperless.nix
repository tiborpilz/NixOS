{ inputs, lib, pkgs, ... }:

let
  paperlessPort = 8010;
  my = import ../lib { inherit inputs lib pkgs; };
in
{
  name = "paperless-test";

  nodes.machine = { config, pkgs, lib, ... }: {
    inherit lib;

    virtualisation.diskSize = 8192;

    nix.useSandbox = false;

    imports = [
      (import ../modules/nixos/podgroups.nix { inherit config pkgs; lib = lib // { inherit my; }; })
      (import ../modules/nixos/services/reverseProxy.nix { inherit config pkgs; lib = lib // { inherit my; }; })
      (import ../modules/nixos/services/paperless-ng.nix { inherit config pkgs; lib = lib // { inherit my; }; })
    ];

    modules.services.paperless.enable = true;
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("podman-paperless-ngx-webserver.service")
    machine.wait_for_open_port(${toString paperlessPort})
    machine.succeed("curl -sSf http://localhost:${toString paperlessPort}")
  '';
}
