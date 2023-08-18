{ config, lib, pkgs, ... }:
with lib;
{
  home.activation = mkIf pkgs.stdenv.isDarwin {
    copyApplications = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      base_dir="$HOME/Applications/hm-apps"
      if [ -d "$base_dir" ]; then
        rm -rf "$base_dir"
      fi
      mkdir -p "$base_dir"
      for app_file in ${apps}/Applications/*; do
        target="$base_dir/$(basename "$app_file")"
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$app_file" "$base_dir"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done
    '';
  };
}
