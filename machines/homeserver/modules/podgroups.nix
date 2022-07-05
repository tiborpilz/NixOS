{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.podgroups;
  dbOptions = { ... }: { options = { enable = mkEnableOption "database"; }; };

  podOptions = { ... }: {
    options = {
      port = mkOption {
        type = types.str;
        description = "Port binding";
        example = "80:8080";
      };

      containers = mkOption {
        default = { };
        description = "containers";
      };
    };
  };

  podContainerNames = podName: pod:
    map (containerName: "${podName}-${containerName}")
      (attrNames pod.containers);
  recursiveMergeAttrs = listOfAttrsets:
    lib.fold (attrset: acc: lib.recursiveUpdate attrset acc) { } listOfAttrsets;

  mkDb = db: {
    image = "postgres";
    environment = {
      "POSTGRES_USER" = if hasAttr "user" db then db.user else "postgres";
      "POSTGRES_PASSWORD" =
        if hasAttr "password" db then db.password else "postgres";
      "POSTGRES_DB" = if hasAttr "db" db then db.db else "postgres";
    };
  };

  mkService = name: pod: {
    serviceConfig.Type = "oneshot";
    wantedBy = map (containerName: "podman-${containerName}.service")
      (podContainerNames name pod);
    script = ''
      ${pkgs.podman}/bin/podman pod exists ${name}-pod || \
      ${pkgs.podman}/bin/podman pod create -n ${name}-pod -p '${pod.port}'
    '';
  };

  mkContainers = pods:
    recursiveMergeAttrs (concatMap
      (podName:
        let
          pod = pods."${podName}";
          renameDependencies = dependsOn:
            map (dependencyName: "${podName}-${dependencyName}") dependsOn;
        in
        map
          (containerName:
            let
              pod = pods."${podName}";
              container = pod.containers."${containerName}";
              podOption = { extraOptions = [ "--pod=${podName}-pod" ]; };
              adjustedDependsOn =
                if hasAttr "dependsOn" container then {
                  dependsOn = renameDependencies container.dependsOn;
                } else
                  { };
            in
            {
              "${podName}-${containerName}" = (container // podOption)
                // adjustedDependsOn;
            })
          (attrNames pod.containers))
      (attrNames pods));

in
{
  options.podgroups = {
    pods = mkOption {
      default = { };
      type = types.attrsOf (types.submodule podOptions);
      description = "Podman pods";
    };
  };

  config = mkIf (cfg.pods != { }) (mkMerge [
    {
      systemd.services = mapAttrs'
        (n: v: nameValuePair "podman-create-pod-${n}-pod" (mkService n v))
        cfg.pods;
    }
    {
      virtualisation.oci-containers.containers = (mkContainers cfg.pods);
    }
    # { virtualisation.oci-containers.containers = (mkContainers cfg.pods); }
  ]);
}
