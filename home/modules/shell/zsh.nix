{ inputs, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.shell.zsh;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  configDir = ../../config;
  envVars = config.home.sessionVariables;
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
      eza
      fasd
      fd
      fzf
      jq
      ripgrep
      gron
      tldr
      kubectl
      krew
      nodejs
      coreutils
      wakatime

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

      # thefuck initialization
      # eval $(thefuck --alias)
    '';

    modules.shell.zsh.aliases.xclip = "xclip -selection clipboard";

    modules.shell.zsh.aliases.nix-shell = "nix-shell --run zsh";

    xdg.configFile."zsh" = { source = "${configDir}/zsh"; recursive = true; };

    xdg.configFile."zsh/extra.zshrc".text =
      let aliasLines = mapAttrsToList (n: v: "alias ${n}=\"${v}\"") cfg.aliases;
      in ''
        # This file was autogenerated, do not edit it!
        ${concatStringsSep "\n" aliasLines}
         ${concatMapStrings (path: "source '${path}'\n") cfg.rcFiles}
         ${cfg.rcInit}
      '';

    xdg.configFile."zsh/extra.zshenv".text = ''
      # This file is autogenerated, do not edit it!
      ${concatMapStrings (path: "source '${path}'\n") cfg.envFiles}
       ${cfg.envInit}
    '';

    # Picks up all files in all `home.packages` that contain '#compdef' and puts them into one completion directory.
    # xdg.configFile."zsh/vendor-completions".source = with pkgs;
    #   runCommandNoCC "vendored-zsh-completions" {} ''
    #     mkdir -p $out
    #     ${fd}/bin/fd -t f '^_[^.]+$' \
    #       ${lib.escapeShellArgs home.packages} \
    #       --exec ${ripgrep}/bin/rg -0l '^#compdef' {} \
    #       | xargs -0 cp -n -t $out/
    #   '';
  };
}
