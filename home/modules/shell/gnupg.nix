{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.gnupg;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.gnupg = with types; {
    keyid = mylib.mkOpt types.str ""; # public key id
    keygrip = mylib.mkOpt types.str ""; # keygrip
    enable = mylib.mkBoolOpt false;
    cacheTTL = mylib.mkOpt int 3600; # 1hr
    maxCacheTTL = mylib.mkOpt int 86400; # 1 day
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.yubikey-personalization
      pkgs.yubikey-manager
      pkgs.gnupg
    ];

    home.file."isDarwin" = lib.mkIf pkgs.stdenv.isDarwin {
      text = ''
        is darwin!
      '';
    };

    # For some reason, `programs.gpg` doesn't work on mac, although there
    # is a gnupg package. So I use the package and configure the package directly.
    # programs.gpg.enable = true;
    # services.gpg-agent.enable = true;
    # services.gpg-agent.enableSshSupport = true;

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
        pcsc-driver /usr/lib/libpcsclite.so
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
