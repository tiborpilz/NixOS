{ config, options, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.snapclient;
  mylib = import ../../lib { inherit inputs pkgs lib; };
in
{
  options.modules.snapclient = {
    enable = mylib.mkBoolOpt false;

    host = mkOption {
      type = types.str;
      default = "klaus";
      description = "Snapcast server, i.e. Music Assistant's built-in snapserver";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ pkgs.snapcast ];
    }

    # launchd options only exist on darwin, so the agent has to stay out of the
    # config attrset entirely on linux
    (optionalAttrs (options ? launchd) {
      launchd.agents.snapclient = {
        enable = true;
        config = {
          ProgramArguments = [ "${pkgs.snapcast}/bin/snapclient" "--host" cfg.host ];
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${config.home.homeDirectory}/Library/Logs/snapclient.log";
          StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/snapclient.err.log";
        };
      };
    })
  ]);
}
