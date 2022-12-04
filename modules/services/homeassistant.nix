{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  homeassistant_cfg = {
    default_config = { };
    automation = [ ];
    script = [ ];
    scene = { };
    homeassistant = {
      name = "Tibitin Nest";
      latitude = "52.51405";
      longitude = "13.48078";
      elevation = 52;
      unit_system = "metric";
      time_zone = "Europe/Berlin";
    };
    deconz = {
      host = "192.168.2.31";
      port = "80";
      api_key = "393D3B904C";
    };
  };

  homeassistant_yaml = generators.toYAML { } homeassistant_cfg;

  homeassistant_config_dir = "/var/lib/homeassistant/config";
  cfg = config.modules.services.homeassistant;
in
{
  options.modules.services.homeassistant = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.generateHomeassistantConfig = stringAfter [ "var" ] ''
      mkdir -p ${homeassistant_config_dir}
      echo '${homeassistant_yaml}' > ${homeassistant_config_dir}/configuration.yaml
    '';

    virtualisation.oci-containers.containers.homeassistant = {
      image = "ghcr.io/home-assistant/home-assistant:2020.1.0";
      volumes = [ "${homeassistant_config_dir}:/config" ];
      environment = { };
      extraOptions = [
        "--network=host"
      ];
    };
  };
}
