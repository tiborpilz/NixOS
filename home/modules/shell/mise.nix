{ inputs, config, lib, pkgs, ... }:

# mise (previously rtx) is a tool for managing dev environments. See also: asdf, direnv
with lib;
let
  cfg = config.modules.shell.mise;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.shell.mise = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.unstable.mise ];
    # Static PATH prepend instead of `mise activate --shims`, which adds a
    # chpwd hook that costs ~270ms on cold shells.
    modules.shell.zsh.envInit = ''
      export PATH="$HOME/.local/share/mise/shims:$PATH"
    '';
    modules.shell.zsh.fpathDirs = "${pkgs.unstable.mise}/share/zsh/site-functions";
  };
}
