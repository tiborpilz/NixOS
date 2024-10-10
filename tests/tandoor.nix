{ inputs, lib, pkgs, ... }:

let
  tandoorPort = 8185;
  my = import ../lib { inherit inputs lib pkgs; };
in
{
  name = "tandoor-test";

  nodes.machine = { config, pkgs, lib, ... }: {
    inherit lib;

    virtualisation.diskSize = 8192;

    # nix.useSandbox = false;

    imports = [
      (import ../modules/nixos/podgroups.nix { inherit config pkgs; lib = lib // { inherit my; }; })
      (import ../modules/nixos/services/reverseProxy.nix { inherit config pkgs; lib = lib // { inherit my; }; })
      (import ../modules/nixos/services/tandoor.nix { inherit config pkgs; lib = lib // { inherit my; }; })
    ];

    modules.services.tandoor.enable = true;
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("podman-tandoor-tandoor.service")
    machine.wait_for_open_port(${toString tandoorPort})
    machine.succeed("curl -sSf http://localhost:${toString tandoorPort}")
  '';
}
