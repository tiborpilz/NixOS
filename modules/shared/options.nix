{ config, options, lib, ... }:

with lib;
with lib.my;
{
  options = with types; {
    user = mkOpt attrs { };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v:
          if isList v
          then concatMapStringSep ":" (x: toString x) v
          else (toString v));
      default = { };
    };
  };

  config = {
    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
