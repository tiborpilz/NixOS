{ inputs, config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.gnupg;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.gnupg = with types; {
    enable = mylib.mkBoolOpt false;
    cacheTTL = mylib.mkOpt int 3600; # 1hr
    maxCacheTTL = mylib.mkOpt int 86400; # 1 day
    keygrip = mylib.mkOpt string "Keygrip = 1050A7CD50EAFCD36E696470775BC39D6FFA47A4";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.yubikey-personalization
      pkgs.yubikey-manager
      pkgs.gnupg
    ];

    # For some reason, `programs.gpg` doesn't work on mac, although there
    # is a gnupg package. So I use the package directly.
    # programs.gpg.enable = true;
    # services.gpg-agent.enable = true;
    # services.gpg-agent.enableSshSupport = true;

    home.file.".gnupg/gpg-agent.conf" = {
      text = ''
        default-cache-ttl ${toString cfg.cacheTTL}
        pinentry-program ${pkgs.pinentry.gtk2}/bin/pinentry
        enable-ssh-support
        write-env-file
        use-standard-socket
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
        ${cfg.keygrip}
      '';
    };

    # Tell SSH to use gpg-agent
    modules.shell.zsh.rcInit = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      gpgconf --launch gpg-agent
      echo UPDATESTARTUPTTY | gpg-connect-agent
    '';
  };
}
