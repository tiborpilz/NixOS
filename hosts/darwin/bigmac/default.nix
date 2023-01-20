{ pkgs, lib, ... }:
with lib;
{
  # Compatibility workaround, some home modules set systemd which is not available on darwin
  options = {
    # allow any object
    systemd = mkOption {
      type = types.attrs;
      default = {};
      description = ''
        Systemd configuration. See <citerefentry>
        <refentrytitle>systemd</refentrytitle>
        <manvolnum>5</manvolnum>
        </citerefentry> for details.
      '';
    };
  };

  config = {
    homebrew.enable = true;

    homebrew.casks = [ "flutter" ];
    networking = {
      dns = [ "1.1.1.1" "8.8.8.8" ];
    };

    services.nix-daemon.enable = true;
    services.karabiner-elements.enable = true;

    programs.zsh.enable = true;

    users.users."tibor.pilz" = {
      shell = pkgs.zsh;
      description = "Tibor Pilz";
      home = "/Users/tibor.pilz";
    };
  };
}
