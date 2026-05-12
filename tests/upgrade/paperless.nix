{ inputs, lib, pkgs, ... }:

let
  port = 8013;
  appUnit = "paperless-ngx-webserver.service";
  dbUnit = "paperless-ngx-db.service";
  oldImage = "ghcr.io/paperless-ngx/paperless-ngx:2.13.5";
  verifyPath = "/api/documents/";

  my = import ../../lib { inherit inputs lib pkgs; };
in
{
  name = "paperless-upgrade-test";

  nodes.machine = { config, pkgs, lib, ... }: {
    inherit lib;

    virtualisation.diskSize = 16384;
    virtualisation.cores = 2;
    virtualisation.memorySize = 6144;

    nix.settings.sandbox = false;

    imports = [
      inputs.sops-nix.nixosModules.sops
      inputs.quadlet-nix.nixosModules.quadlet
      (import ../../modules/nixos/services/reverseProxy.nix {
        inherit config pkgs; lib = lib // { inherit my; };
      })
      (import ../../modules/nixos/services/paperless-ng.nix {
        inherit config pkgs; lib = lib // { inherit my; };
      })
    ];

    modules.services.paperless.enable = true;

    specialisation.old.configuration = {
      virtualisation.quadlet.containers.paperless-ngx-webserver.containerConfig.image =
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
    machine.wait_for_open_port(${toString port}, timeout=900)
    machine.sleep(60)  # paperless first-boot migrations are slow

    # Phase 2: switch back to the default (NEW) image against the existing pgdata.
    print("=== Phase 2: NEW image (default) ===")
    machine.succeed("/run/booted-system/bin/switch-to-configuration test")
    machine.wait_for_unit("${dbUnit}", timeout=600)
    machine.wait_for_unit("${appUnit}", timeout=600)
    machine.wait_for_open_port(${toString port}, timeout=900)
    machine.sleep(60)  # let migrations run against existing schema

    # Phase 3: hit a DB-backed endpoint.
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
