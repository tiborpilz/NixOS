{ ... }:
{
  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.tandoor-db = {
    image = "postgres:11";
    ports = [ ];
    volumes = [ ];
    environment = { };
  };
}
