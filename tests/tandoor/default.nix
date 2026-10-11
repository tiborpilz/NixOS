# End-to-end upgrade test for the tandoor module.
#
# The baseline version (the one currently deployed, see images.nix) creates a
# database with real data. The VM then switches to the target version (the
# module's image by default) against the same pgdata volume, so the target's
# migrations run, and verifies the migration state, the web UI and the data.
#
# `sabotage` deliberately breaks the upgrade; those variants must fail, which
# proves the checks are able to catch a broken upgrade.
{ inputs, lib, pkgs, target ? null, targetDb ? null, extraPins ? { }, sabotage ? null }:

let
  port = 8285;
  pinned = import ./images.nix;
  my = import ../../lib { inherit inputs lib pkgs; };
  myLib = lib // { inherit my; };

  # Only the option defaults are read, so the module needs no real config.
  moduleOptions = (import ../../modules/nixos/services/tandoor.nix {
    config = { };
    inherit pkgs;
    lib = myLib;
  }).options.modules.services.tandoor;

  baseline = pinned.baseline;
  targetImage = if target == null then moduleOptions.image.default else target;
  # The database image changes with the upgrade as well (a major postgres bump
  # would not start on the old data directory).
  baselineDb = pinned.baselineDb;
  dbImage = if targetDb == null then moduleOptions.dbImage.default else targetDb;
  isUpgrade = baseline != targetImage;

  splitRef = ref:
    let m = builtins.match "(.+):([^:/]+)" ref;
    in if m == null then throw "tandoor test: image '${ref}' has no tag" else {
      name = builtins.elemAt m 0;
      tag = builtins.elemAt m 1;
    };

  pull = ref:
    let
      pin = (pinned.pins // extraPins).${ref} or (throw ''
        tandoor test: no pinned image for ${ref}.
        Run tests/tandoor/update-images.sh to pin it in tests/tandoor/images.nix.
      '');
    in
    pkgs.dockerTools.pullImage (pin // {
      finalImageName = (splitRef ref).name;
      finalImageTag = (splitRef ref).tag;
      os = "linux";
      arch = "amd64";
    });

  imageTars = map pull (lib.unique [ baselineDb dbImage baseline targetImage ]);

  e2e = pkgs.writeShellScriptBin "tandoor-e2e" ''
    exec ${pkgs.python3}/bin/python3 ${./e2e.py} "$@"
  '';

  sabotages = [ null "migration" "web" ];
in
assert lib.assertMsg (builtins.elem sabotage sabotages)
  "tandoor test: unknown sabotage '${toString sabotage}'";
{
  name = "tandoor-upgrade" + lib.optionalString (sabotage != null) "-sabotage-${sabotage}";

  # Two full tandoor boots, migrations and collectstatic; without KVM this is slow.
  globalTimeout = 10 * 3600;

  nodes.machine = { config, lib, ... }: {
    imports = [
      inputs.sops-nix.nixosModules.sops
      inputs.quadlet-nix.nixosModules.quadlet
      (import ../../modules/nixos/services/reverseProxy.nix { inherit config; lib = myLib; })
      (import ../../modules/nixos/services/tandoor.nix { inherit config pkgs; lib = myLib; })
    ];

    virtualisation = {
      diskSize = 20480;
      memorySize = 4096;
      cores = 2;
    };

    environment.systemPackages = [ e2e pkgs.curl ];

    modules.services.tandoor = {
      enable = true;
      image = baseline;
      dbImage = baselineDb;
    };

    # Started by the test script once the system bus is up (see below), not at boot.
    virtualisation.quadlet.pods.tandoor-pod.autoStart = false;
    virtualisation.quadlet.containers = {
      tandoor.autoStart = false;
      tandoor-db.autoStart = false;
      # Everything comes from the preloaded archives; never reach for a registry.
      tandoor.containerConfig.pull = lib.mkForce "never";
      tandoor-db.containerConfig.pull = lib.mkForce "never";
      # First requests after boot can exceed gunicorn's 30s default under TCG.
      tandoor.containerConfig.environments.GUNICORN_TIMEOUT = "300";
    };

    systemd.services.tandoor-test-load-images = {
      description = "Load pinned tandoor test images into podman";
      before = [ "tandoor.service" "tandoor-db.service" "tandoor-pod-pod.service" ];
      requiredBy = [ "tandoor.service" "tandoor-db.service" "tandoor-pod-pod.service" ];
      path = [ config.virtualisation.podman.package ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = lib.concatMapStrings (tar: "podman load -i ${tar}\n") imageTars;
    };

    specialisation.target.configuration = {
      modules.services.tandoor.image = lib.mkForce targetImage;
      modules.services.tandoor.dbImage = lib.mkForce dbImage;
      virtualisation.quadlet.containers.tandoor.containerConfig.environments =
        lib.mkIf (sabotage == "web") {
          # The app can never reach its database: nginx answers, gunicorn never does.
          POSTGRES_PORT = lib.mkForce "1";
        };
    };
  };

  testScript = ''
    import shlex

    baseline = "${baseline}"
    target = "${targetImage}"
    is_upgrade = "${lib.boolToString isUpgrade}" == "true"
    sabotage = "${toString sabotage}"


    def fail(tag, msg):
        raise Exception("E2E-" + "FAIL" + f"[{tag}] {msg}")


    def e2e(args, timeout=4 * 3600):
        status, out = machine.execute(f"tandoor-e2e {args} 2>&1", timeout=timeout)
        print(out)
        if status != 0:
            raise Exception(f"tandoor-e2e {args.split()[0]} exited with {status}")
        return out


    def ensure_pid1_on_bus():
        # Without KVM, PID 1 occasionally loses the race to connect to the freshly
        # started dbus-broker during boot and never gets back on the system bus;
        # podman then cannot create the pod's cgroup. Re-exec reconnects it.
        machine.wait_for_unit("dbus.service")
        if machine.execute("busctl status org.freedesktop.systemd1")[0] != 0:
            print("PID 1 is not on the system bus, re-executing systemd")
            machine.succeed("systemctl daemon-reexec")
            machine.wait_until_succeeds("busctl status org.freedesktop.systemd1", timeout=300)


    def run(cmd):
        status, out = machine.execute(f"{cmd} 2>&1", timeout=1800)
        print(out)
        if status != 0:
            print(f"{cmd} exited with {status}")


    def container_id():
        return machine.execute("podman inspect tandoor --format '{{.Id}}'")[1].strip()


    machine.start()

    with subtest(f"baseline {baseline} creates the database"):
        machine.wait_for_unit("multi-user.target", timeout=3600)
        ensure_pid1_on_bus()
        machine.succeed("systemctl start --no-block tandoor-pod-pod.service")
        machine.wait_for_unit("tandoor-test-load-images.service", timeout=3600)
        machine.wait_for_unit("tandoor-db.service", timeout=1800)
        machine.wait_for_unit("tandoor.service", timeout=1800)
        e2e("wait-ready --db-container tandoor-db --timeout 7200", timeout=7500)
        e2e("seed")
        e2e(f"verify --expect-image {baseline}")

    if is_upgrade:
        with subtest(f"control: {target} sees pending migrations on the baseline database"):
            e2e(f"control-pending --image {target}")

    if sabotage == "migration":
        with subtest("sabotage: forget the cookbook migration history"):
            machine.succeed(
                "podman exec tandoor-db psql -U recipes -d recipes -c "
                + shlex.quote("DELETE FROM django_migrations WHERE app = 'cookbook'")
            )

    with subtest(f"switch to {target}"):
        before = container_id()
        # A broken upgrade may make these fail; the checks below say why.
        run("/run/booted-system/specialisation/target/bin/switch-to-configuration test")
        if not is_upgrade:
            # Same version: still restart it against the existing database.
            run("systemctl restart tandoor.service")

    with subtest(f"{target} runs on the migrated database"):
        ready_timeout = 1800 if sabotage == "web" else 7200
        e2e(f"wait-ready --db-container tandoor-db --timeout {ready_timeout}", timeout=ready_timeout + 300)
        if container_id() == before:
            fail("switch", "tandoor container was not recreated")
        e2e(f"verify --expect-image {target}")
  '';
}
