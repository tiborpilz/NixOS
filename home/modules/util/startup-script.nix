{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.startup-script;

  scriptOptions = { ... }: {
    options = {
      script = mkOption {
        type = types.str;
        default = "";
        description = "The startup script";
      };
    };
  };

  createScriptPkg = name: script: pkgs.writeScriptBin "startup-script-${name}" ''
    ${script}
  '';
in
{
  options.modules.startup-script = mkOption {
    type = types.attrsOf types.str;
    default = {};
  };

  config = mkMerge [
    # Duck type for darwin
    (if (builtins.hasAttr "launchd" options) then {
      launchd.agents = mkMerge (mapAttrsToList (name: opts:
        let
          scriptPkg = createScriptPkg name opts.script;
        in
        {
          "startup-${name}" = {
            program = "${scriptPkg}/bin/startup-script-${name}";
            keepAlive = true;
            runAtLoad = true;
          };
        }) cfg);
    } else {
      systemd.user.services = mkMerge (mapAttrsToList (name: script:
        let
          scriptPkg = createScriptPkg name script;
        in
        {
          "startup-${name}" = {
            Unit = {
              Description = "Startup script ${name}";
            };
            Service = {
              Type = "oneshot";
              ExecStart = "${scriptPkg}/bin/startup-script-${name}";
              RemainAfterExit = true;
            };
          };
        }) cfg);
    })
  ];
}
