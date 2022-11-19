{ config, options, inputs, lib, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.home;
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  options.home = with types; {
    enable = mkBoolOpt true;
    file = mkOpt' attrs {} "Files to place directly in $HOME";
    configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
  };

  config = mkIf cfg.enable {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.tibor.imports = [ ../home ];

    home-manager.users.tibor.modules.syncthing.service = true;

    home-manager.users.tibor.home.file = mkAliasDefinitions options.home.file;
    home-manager.users.tibor.xdg.configFile = mkAliasDefinitions options.home.configFile;

    systemd.services.home-manager-tibor = {
      # Need to wait for network since home-manager will get stuff from git
      after = [ "network-online.target" ];
    };
  };
}
