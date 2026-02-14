{ inputs, config, lib, pkgs, ... }:

with lib;
with builtins;
let
  cfg = config.modules.shell.git;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # TODO: make this configurable
  # gitConfigSource = ../../config/git;
  # gitConfigLinkSource = "${config.home.homeDirectory}/Code/nixos/home/config/git";
  # gitFiles = attrNames (readDir gitConfigSource);
  # gitConfigFiles = listToAttrs (map (file: {
  #   name = "git/${file}";
  #   value = {
  #     source = config.lib.file.mkOutOfStoreSymlink "${gitConfigLinkSource}/${file}";
  #     recursive = true;
  #   };
  # }) gitFiles);
in
{
  options.modules.shell.git = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      git
      git-cliff
      git-absorb
      gh
      glab
      git-open
      diff-so-fancy
      git-crypt
      difftastic
      act
      lazygit
      unstable.opencommit
    ];

    xdg.configFile = {
      "git/config".source = ../../config/git/config;
      "git/config_liqid".source = ../../config/git/config_liqid;
      "git/ignore".source = ../../config/git/ignore;
      "git/attributes".source = ../../config/git/attributes;
      "git/gitk".source = ../../config/git/gitk;
      "git/ignore_global".source = ../../config/git/ignore_global;
    };

    modules.shell.zsh.fpathDirs = ''
      ${pkgs.gitAndTools.gh}/share/zsh/site-functions
      ${pkgs.git-absorb}/share/zsh/site-functions
    '';

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
