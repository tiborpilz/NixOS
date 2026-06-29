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
      git-absorb # who needs mercurulial
      unstable.git-spice # Stacked Diffs

      # CLIs for varios free and non-free (*cough*) forges
      gh
      glab

      git-open
      git-crypt # Stop committing keys, dummy

      diff-so-fancy #nicer git diffs
      difftastic # nicer git diffs with treesitter

      act # GH actions at home
      lazygit # for noobs
      unstable.opencommit

      # Worktree Management
      my.wtp
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

    # wtp completion + shell integration (the `wtp cd` navigation hook)
    modules.shell.zsh.rcInit = ''
      eval "$(${pkgs.my.wtp}/bin/wtp shell-init zsh)"
    '';
  };
}
