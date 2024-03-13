{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.rust;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.rust = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    # Packages for rust development
    home.packages = with pkgs; [
      unstable.rustup # contains rustc, cargo, rustdoc, rustfmt, etc.
      cargo-watch # cargo subcommand for watching files and running commands
      cargo-make # cargo subcommand for running tasks defined in a toml file
    ];
  };
}
