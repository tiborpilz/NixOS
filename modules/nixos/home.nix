{ config, options, inputs, lib, ... }:

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
    graphical = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    # Back up (rather than abort on) any pre-existing unmanaged file that HM
    # wants to own — e.g. a stale real 10-hm-fonts.conf left by an older
    # generation. Conflicting files are moved aside to <name>.backup.
    home-manager.backupFileExtension = "backup";
    home-manager.sharedModules = [
      inputs.plasma-manager.homeModules.plasma-manager
      # inputs.nix-doom-emacs-unstraightened.homeModule
    ];
    # home-manager.extraSpecialArgs = { inherit lib; };
    home-manager.users.tibor = mkMerge [
      inputs.nix-doom-emacs-unstraightened.hmModule
      {
        _module.args.inputs = inputs;
        _module.args.lib = lib;
        imports = [ ../../home ];
        home.file = mkAliasDefinitions options.home.file;
        xdg.configFile = mkAliasDefinitions options.home.configFile;
        graphical = cfg.graphical;
      }
    ];


    systemd.services.home-manager-tibor = {
      # Need to wait for network since home-manager will get stuff from git
      after = [ "network-online.target" ];
    };
  };
}
