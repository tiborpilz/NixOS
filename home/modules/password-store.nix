{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.password-store;
  mylib = import ../../lib { inherit inputs lib pkgs; };
  storeDir = "${config.xdg.dataHome}/password-store";
in
{
  options.modules.password-store = {
    enable = mylib.mkBoolOpt false;
    enable-sync = mylib.mkBoolOpt false;
    gpg-id = mylib.mkOpt types.str config.modules.shell.gnupg.public_key;
  };

  config.home.packages = mkIf cfg.enable [
    pkgs.pass2csv
    pkgs.bitwarden-cli
    pkgs.my.bw2pass # custom script that imports bitwarden to pass
  ];

  config.programs.password-store = mkIf cfg.enable {
    enable = true;
    # keep the pre-26.05 default since the store already lives here
    settings = { PASSWORD_STORE_DIR = storeDir; };
  };

  # `pass` refuses to work without a store, so create an empty one on the first
  # activation that finds the key. Ordered after the gnupg module's key import,
  # which is what puts the public key in the keyring.
  config.home.activation.initPasswordStore = mkIf (cfg.enable && cfg.gpg-id != "")
    (lib.hm.dag.entryAfter [ "writeBoundary" "importGpgKeys" ] ''
      if [ ! -e "${storeDir}/.gpg-id" ]; then
        if ${pkgs.gnupg}/bin/gpg --list-keys ${cfg.gpg-id} > /dev/null 2>&1; then
          $DRY_RUN_CMD env PASSWORD_STORE_DIR="${storeDir}" \
            ${config.programs.password-store.package}/bin/pass init ${cfg.gpg-id}
        else
          echo "password-store: skipping init, public key ${cfg.gpg-id} is not in the keyring"
        fi
      fi
    '');

  config.services.password-store-sync = mkIf (cfg.enable && cfg.enable-sync) {
    enable = true;
  };
}
