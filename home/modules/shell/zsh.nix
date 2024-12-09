{ inputs, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.shell.zsh;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  configDir = ../../config;
  envVars = config.home.sessionVariables;

  mkFpathEntries = paths: ''
    ${concatMapStrings (path: "fpath+=(${path})\n") (strings.splitString "\n" paths)}
  '';
in
{
  options.modules.shell.zsh = with types; {
    enable = mylib.mkBoolOpt false;

    aliases = mylib.mkOpt (attrsOf (either str path)) { };

    rcInit = mylib.mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshrc and sourced by $XDG_CONFIG_HOME/zsh/.zshrc
    '';

    envInit = mylib.mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshenv and sourced by $XDG_CONFIG_HOME/.config/zsh/.zshenv
    '';

    fpathDirs = mylib.mkOpt' lines "" ''
      Zsh fpath entries to be added to $fpath
    '';
    rcFiles = mylib.mkOpt' (listOf (either str path)) [ ] ''
      Zsh files to be sourced by $XDG_CONFIG_HOME/zsh/.zshrc
    '';

    envFiles = mylib.mkOpt' (listOf (either str path)) [ ] ''
      Zsh files to be sourced by $XDG_CONFIG_HOME/zsh/.zshenv
    '';
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
    };

    home.packages = with pkgs; [
      zsh
      zsh-completions
      zsh-syntax-highlighting
      nix-zsh-completions
      eza # drop-in replacement for ls
      fd
      fzf
      jq
      yq
      ripgrep
      gron
      tldr
      kubectl
      krew
      nodejs
      coreutils

      thefuck

      #Markdown View
      glow
    ];

    home.sessionVariables.ZDOTDIR = "${envVars.XDG_CONFIG_HOME}/zsh";
    home.sessionVariables.ZSH_CACHE_DIR = "${envVars.XDG_CACHE_HOME}/zsh";
    # home.sessionVariables.NVM_DIR = "${envVars.XDG_CONFIG_HOME}/nvm";

    # home.sessionPath = [ "${home.sessionVariables.XDG_CONFIG_HOME}/.krew/bin" ];

    modules.shell.zsh.rcInit = ''
      # completion
      source ${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/nix-zsh-completions.plugin.zsh
      fpath=(${pkgs.nix-zsh-completions}/share/zsh/site-functions $fpath)
      fpath=(${pkgs.nix}/share/zsh/site-functions $fpath)
      fpath=(${pkgs.zsh-completions}/share/zsh/site-functions $fpath)
    '';

    programs.thefuck.enable = true;

    modules.shell.zsh.aliases = {
      xclip = "xclip -selection clipboard";
      nix-shell = "nix-shell --run zsh"; # keep using zsh in nix shell

      # speeds up non-scoped npm installs
      nci = "npm_config_registry=https://registry.npmjs.org/ npm ci";

      # Tools that can be used as drop-in replacements
      # ls = "eza";
    };

    xdg.configFile."zsh" = { source = "${configDir}/zsh"; recursive = true; };

    xdg.configFile."zsh/extra.zshrc".text =
      let aliasLines = mapAttrsToList (n: v: "alias ${n}=\"${v}\"") cfg.aliases;
      in ''
        # This file was autogenerated, do not edit it!
        ${concatStringsSep "\n" aliasLines}
        ${concatMapStrings (path: "source '${path}'\n") cfg.rcFiles}
        ${mkFpathEntries cfg.fpathDirs}
        ${cfg.rcInit}
      '';

    xdg.configFile."zsh/extra.zshenv".text = ''
      # This file is autogenerated, do not edit it!
      ${concatMapStrings (path: "source '${path}'\n") cfg.envFiles}
       ${cfg.envInit}
    '';
  };
}
