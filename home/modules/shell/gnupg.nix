{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.gnupg;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.gnupg = with types; {
    public_key = mylib.mkOpt str ""; # public key id
    keygrip = mylib.mkOpt types.str ""; # keygrip
    enable = mylib.mkBoolOpt false;
    cacheTTL = mylib.mkOpt int 3600; # 1hr
    maxCacheTTL = mylib.mkOpt int 86400; # 1 day
  };

  config = mkIf cfg.enable {
    # For some reason, `programs.gpg` doesn't work on mac, although there
    # is a gnupg package. So I use the package and configure everything else system-agnostic.

    home.packages = [
      pkgs.yubikey-personalization
      pkgs.yubikey-manager
      pkgs.gnupg
    ];

    # Automatically import public key from keyserver and, if connected, yubikey
    home.activation = {
      importGpgKeys =
        let
          gpg = "${pkgs.gnupg}/bin/gpg";
          keyid = cfg.public_key;
        in
        mkIf (cfg.public_key != "")
          (lib.hm.dag.entryAfter [ "linkGeneration" ] ''
            ${gpg} --list-keys ${keyid} > /dev/null 2>&1 || ${gpg} --recv-keys ${keyid} > /dev/null 2>&1 || echo "Error during gpg import: No key!"
            ${gpg} --list-secret-keys ${keyid} > /dev/null 2>&1 || ${gpg} --card-status > /dev/null 2>&1 || echo "Error during gpg import: No card!"
          '');
    };

    home.file.".gnupg/gpg-agent.conf" = {
      text = ''
        default-cache-ttl ${toString cfg.cacheTTL}
        pinentry-program ${pkgs.pinentry.gtk2}/bin/pinentry
        enable-ssh-support
        default-cache-ttl-ssh ${toString cfg.cacheTTL}
        max-cache-ttl ${toString cfg.maxCacheTTL}
      '';
    };
    home.file.".gnupg/scdaemon.conf" = {
      text = ''
        card-timeout 5
        disable-ccid
      '';
    };
    # SSH Key from Yubikey
    home.file.".gnupg/sshcontrol" = {
      text = ''
        Keygrip = ${cfg.keygrip}
      '';
    };

    # Tell SSH to use gpg-agent
    modules.shell.zsh.rcInit = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      gpgconf --launch gpg-agent
      echo UPDATESTARTUPTTY | gpg-connect-agent > /dev/null
    '';
  };
}
