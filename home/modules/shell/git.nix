{ inputs, config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.git;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.git = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      git
      gitAndTools.git-annex
      gitAndTools.gh
      gitAndTools.glab
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
