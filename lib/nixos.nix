{ inputs, lib, ... }:

with lib;
with lib.my;
let sys = "x86_64-linux";
in {
  mkHostAttrs = path: attrs @ { system ? sys, ... }: {
    inherit system;
    specialArgs = { inherit lib inputs system; };
    modules = [
      {
        networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
      }
      (filterAttrs (n: v: !elem n [ "system" ]) attrs)
      ../. # /default.nix
      (import path)
    ];
  };

  mkHost = path: attrs @ { system ? sys, ... }:
    nixosSystem mkHostAttrs path attrs;

  mapHosts = dir: attrs @ { system ? system, ... }:
    mapModules dir
      (hostPath: mkHost hostPath attrs);

  mkHomeAliases = name: nixosConfigurations: homeConfigurations:
    mergeAttrs (forEach (attrNames nixosConfigurations) (host: {
      "${name}@${host}" = homeConfigurations."${name}";
    }));
}
