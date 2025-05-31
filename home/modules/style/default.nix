{ inputs, pkgs, lib, config, ... }:
with lib;

let
  mylib = import ../lib { inherit inputs lib pkgs; };
in
{
  stylix.enable = true;
  stylix.autoEnable = true;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
  programs.zathura.enable = true;

  stylix.fonts.sizes.applications = 16;
  stylix.targets.kitty.enable = true;
  stylix.targets.kitty.variant256Colors = true;

  stylix.targets.firefox.profileNames = [ "nixboi" ];
  stylix.targets.firefox.colorTheme.enable = true;
}

