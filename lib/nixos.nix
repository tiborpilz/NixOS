{ inputs, lib , ... }:

with lib;
with lib.my;
let sys = "x86_64-linux";
in {
  mkHostAttrs = path: attrs @ { system ? sys, ... }:
    let isDarwin = system == "x86_64-darwin";
    in {
      inherit system;
      output = if isDarwin then "darwinConfigurations" else "nixosConfigurations";
      builder = if isDarwin then inputs.nix-darwin.lib.darwinSystem else lib.nixosSystem;
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

  mkHomeAliases = name: nixosConfigurations: homeConfigurations:
    mergeAttrs (forEach (attrNames nixosConfigurations) (host: {
      "${name}@${host}" = homeConfigurations."${name}";
    }));
}
