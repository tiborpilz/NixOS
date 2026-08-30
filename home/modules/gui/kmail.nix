{ inputs, lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.gui.kmail;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.gui.kmail = {
    enable = mylib.mkBoolOpt false;
    contacts = mylib.mkBoolOpt true;
    crypto = mylib.mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.kdePackages; [
      kmail

      akonadi
      kdepim-runtime

      kmail-account-wizard
      kdepim-addons
    ]
    ++ optional cfg.contacts pkgs.kdePackages.kaddressbook
    ++ optional cfg.crypto pkgs.kdePackages.kleopatra;
  };
}
