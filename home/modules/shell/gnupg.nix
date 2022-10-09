{ inputs, config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.gnupg;
    mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.shell.gnupg = with types; {
    enable   = mylib.mkBoolOpt false;
    cacheTTL = mylib.mkOpt int 3600;  # 1hr
  };

  config = mkIf cfg.enable {
    home.sessionVariables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gpg.enable = true;

    xdg.configFile."gnupg/gpg-agent.conf" = {
      text = ''
        default-cache-ttl ${toString cfg.cacheTTL}
        pinentry-program ${pkgs.pinentry.gtk2}/bin/pinentry
      '';
    };
  };
}
