{ lib, pkgs, config, inputs, ... }:
with lib;
let
  cfg = config.modules.tools.aws;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.tools.aws.enable = mylib.mkBoolOpt false;

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      awscli2
    ];
  };
}
