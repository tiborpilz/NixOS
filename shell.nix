{ pkgs ? import <nixpkgs> { } }:

let
  isDarwin = pkgs.stdenv.isDarwin;

  # Tools the screenshot scenes need. Kitty itself is intentionally NOT here:
  # nixpkgs Mesa 25 doesn't ship a usable software DRI driver, so we let the
  # system (apt on CI / distro on local Linux) provide Kitty and its libGL.
  screenshotDeps = with pkgs; [
    git
  ] ++ pkgs.lib.optionals (!isDarwin) [
    zsh
    neovim
    tmux
    fzf
    gitmux
    zoxide
    imagemagick
    fontconfig
    xdotool
    emacs
    ripgrep
    fd
    gcc
    nerd-fonts.fira-code
  ];

  commonDeps = with pkgs; [
    git
    nix-zsh-completions
    node2nix
    cachix
    go-task
    deploy-rs
    sops
    nh
    just
    nodejs_22
  ];

  # Screenshots-only: point fontconfig at the nix-installed Nerd Font *and*
  # the system font dir so Kitty resolves "FiraCode Nerd Font Mono" while
  # Emacs still has its usual fallbacks.
  screenshotsShellEnv = pkgs.lib.optionalAttrs (!isDarwin) {
    FONTCONFIG_FILE = pkgs.makeFontsConf {
      fontDirectories = [ pkgs.nerd-fonts.fira-code "/usr/share/fonts" ];
    };
  };
in
{
  default = pkgs.mkShell {
    buildInputs = commonDeps ++ screenshotDeps;
    packages = commonDeps ++ [ pkgs.home-manager ] ++ screenshotDeps;
    shellHook = ''
      export FLAKE="$PWD"
      export NH_FLAKE="$PWD"
      export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
    '';
  };

  screenshots = pkgs.mkShell ({
    buildInputs = screenshotDeps;
    packages = screenshotDeps;
  } // screenshotsShellEnv);
}
