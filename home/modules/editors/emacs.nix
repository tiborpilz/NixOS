# Took a lot of inspiration from hlissner's dotfiles repo
# although some tweaks were necessary, since this is a
# home-manager configuration, not a nixos configuration.

{ lib, pkgs, inputs, ... }:
with lib;


{
  # config = mkIf cfg.enable {
  config = {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    home.packages = with pkgs; [
      ## Emacs
      binutils # for native-comp
      ## 28.2 + native-comp
      ((emacsPackagesFor emacsNativeComp).emacsWithPackages
        (epkgs: [ epkgs.vterm ]))

      ## Doom dependencies
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls

      ## Optional dependencies
      fd # for projectile
      imagemagick # for image-dired
      pinentry-emacs # in-emacs gnupg-prompts
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

      # :tools editorconfig
      editorconfig-core-c

      # :tools lookup & :lang org +roam
      sqlite

      # :lang latex & :lang org (late previews)
      texlive.combined.scheme-medium

      # Fonts
      emacs-all-the-icons-fonts
    ];

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    # modules.shell.zsh.rcFiles = [ "${config.xdg.configHome}/emacs/aliases.zsh" ];

    home.activation.installDoomEmacs = lib.hm.dag.entryAfter ["WriteBoundary"] ''
        if [ ! -d ".config/emacs" ]; then
            git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
        else
            cd ".config/emacs"
            git pull
            cd -
        fi

        if [ ! -d ".config/doom" ]; then
            git clone https://github.com/tiborpilz/doom-emacs-config ".config/doom"
        else
            cd ".config/doom"
            git pull
            cd -
        fi

        .config/emacs/bin/doom sync
      '';
  };
}
