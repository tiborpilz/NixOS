{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gitAndTools.git-annex
      unstable.gitAndTools.gh
      gitAndTools.git-open
      gitAndTools.diff-so-fancy
      (mkIf config.modules.shell.gnupg.enable
        gitAndTools.git-crypt)
      act
    ];

    xdg.configFile = {
      "git/config".source = ../../config/git/config;
      "git/ignore".source = ../../config/git/ignore;
      "git/attributes".source = ../../config/git/attributes;
      "git/gitk".source = ../../config/git/gitk;
    };
  };
}
