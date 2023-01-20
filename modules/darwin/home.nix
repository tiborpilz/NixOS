{ pkgs, config, options, inputs, lib, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.home;
in
{
  imports = [
    inputs.home-manager.darwinModules.home-manager
  ];

  options.home = {
    enable = mkBoolOpt true;
    # file = mkOpt' attrs { } "Files to place directly in $HOME"; # TODO: Maybe replace this if needed?
    # configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
  };

  config = mkIf cfg.enable {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    # home-manager.extraSpecialArgs = { inherit lib; };
    home-manager.users."tibor.pilz" = mkMerge [
      inputs.nix-doom-emacs.hmModule
      {
        _module.args.inputs = inputs;
        _module.args.lib.my = lib.my;
        imports = [ ../../home ];
        # home.file = mkAliasDefinitions options.home.file;
        # xdg.configFile = mkAliasDefinitions options.home.configFile;
      }
    ];
  };
}
