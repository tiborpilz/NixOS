{ pkgs, ... }: {
  homebrew.enable = true;
  networking = rec {
    computerName = "BigMac";
    dns = [ "1.1.1.1" "8.8.8.8" ];
    hostName = computerName;
  };

  services.karabiner-elements.enable = true;
  system.keyboard.remapCapsLockToEscape = true;

  users.users.tiborpilz = {
    createHome = true;
    home = "/Users/tiborpilz";
    shell = pkgs.zsh;
  };
}
