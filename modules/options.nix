{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
{
  options = with types; {
    user = mkOpt attrs {};

    home = {
      file = mkOpt' attrs {} "Files to place directly in $HOME";
      configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v: if isList v
               then concatMapStringSep ":" (x: toString x) v
               else (toString v));
      default = {};
    };
  };

  config = {
    home-manager.users.tibor.home.file = mkAliasDefinitions options.home.file;
    home-manager.users.tibor.xdg.configFile = mkAliasDefinitions options.home.configFile;
    # home-manager.users.tibor.xdg.dataFile = mkAliasDefinitions options.home.file;

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
