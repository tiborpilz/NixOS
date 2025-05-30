{ inputs, config, lib, pkgs, ... }:

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
      git-cliff
      gitAndTools.gh
      gitAndTools.glab
      gitAndTools.git-open
      gitAndTools.diff-so-fancy
      gitAndTools.git-crypt
      difftastic
      act
      lazygit
      unstable.opencommit
    ];

    xdg.configFile = {
      "git/config".source = ../../config/git/config;
      "git/ignore".source = ../../config/git/ignore;
      "git/attributes".source = ../../config/git/attributes;
      "git/gitk".source = ../../config/git/gitk;
      "git/ignore_global".source = ../../config/git/ignore_global;
    };

    modules.shell.zsh.fpathDirs = "${pkgs.gitAndTools.gh}/share/zsh/site-functions";

    modules.shell.zsh.rcInit = ''
      # fpath=(${pkgs.gitAndTools.glab}/share/zsh/site-functions $fpath)
      function opencommit() {
        if [ -z "$OPENAI_API_KEY" ]; then
          # TODO: make key configurable in global config
          export OCO_API_KEY=$(pass bitwarden/openai-api-key)
        else
          export OCO_API_KEY=$OPENAI_API_KEY
        fi
        ${pkgs.unstable.opencommit}/bin/opencommit "$@"
        alias oco="opencommit"
      }
    '';
  };
}
