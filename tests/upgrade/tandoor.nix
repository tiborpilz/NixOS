{ inputs, lib, pkgs, ... }:

let
  port = 8285;
  appUnit = "tandoor.service";
  dbUnit = "tandoor-db.service";
  oldImage = "docker.io/vabene1111/recipes:2.4.0";
  verifyPath = "/api/recipe/";

  my = import ../../lib { inherit inputs lib pkgs; };
in
{
  name = "tandoor-upgrade-test";

  nodes.machine = { config, pkgs, lib, ... }: {
    inherit lib;

    virtualisation.diskSize = 16384;
    virtualisation.cores = 2;
    virtualisation.memorySize = 4096;

    imports = [
      inputs.sops-nix.nixosModules.sops
      inputs.quadlet-nix.nixosModules.quadlet
      (import ../../modules/nixos/services/reverseProxy.nix {
        inherit config pkgs; lib = lib // { inherit my; };
      })
      (import ../../modules/nixos/services/tandoor.nix {
        inherit config pkgs; lib = lib // { inherit my; };
      })
    ];

    modules.services.tandoor.enable = true;

    specialisation.old.configuration = {
      virtualisation.quadlet.containers.tandoor.containerConfig.image =
        lib.mkForce oldImage;
    };
  };

  testScript = ''
    machine.start()

    # Phase 1: activate the OLD image specialisation and let it initialise the DB.
    print("=== Phase 1: OLD image (${oldImage}) ===")
    machine.succeed("/run/booted-system/specialisation/old/bin/switch-to-configuration test")
    machine.wait_for_unit("${dbUnit}", timeout=600)
    machine.wait_for_unit("${appUnit}", timeout=600)
    machine.wait_for_open_port(${toString port}, timeout=600)
    machine.sleep(30)

    # Phase 2: switch back to the default (NEW) image against the existing pgdata.
    print("=== Phase 2: NEW image (default) ===")
    machine.succeed("/run/booted-system/bin/switch-to-configuration test")
    machine.wait_for_unit("${dbUnit}", timeout=600)
    machine.wait_for_unit("${appUnit}", timeout=600)
    machine.wait_for_open_port(${toString port}, timeout=600)
    machine.sleep(30)

    # Phase 3: hit a DB-backed endpoint. Unauthenticated should yield 200/401/403,
    # not 500 (which would indicate a broken router/migration).
    print("=== Phase 3: verify ${verifyPath} ===")
    status = machine.succeed(
      "curl -s -o /dev/null -w '%{http_code}' "
      "http://localhost:${toString port}${verifyPath}"
    ).strip()
    print(f"HTTP status: {status}")
    assert status in ("200", "401", "403"), (
      f"DB-backed endpoint returned {status}; migrations likely broken"
    )
  '';
}
