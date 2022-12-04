{ config, options, inputs, lib, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.home;
in
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  options.home = with types; {
    enable = mkBoolOpt true;
    file = mkOpt' attrs { } "Files to place directly in $HOME";
    configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
  };

  config = mkIf cfg.enable {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    # home-manager.extraSpecialArgs = { inherit lib; };
    home-manager.users.tibor = mkMerge [
      inputs.nix-doom-emacs.hmModule
      {
        _module.args.inputs = inputs;
        _module.args.lib = lib;
        imports = [ ../home ];
        home.file = mkAliasDefinitions options.home.file;
        xdg.configFile = mkAliasDefinitions options.home.configFile;
      }
    ];


    systemd.services.home-manager-tibor = {
      # Need to wait for network since home-manager will get stuff from git
      after = [ "network-online.target" ];
    };
  };
}
