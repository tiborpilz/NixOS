{ pkgs, lib, ... }:
let lang = with pkgs; {
  interpreter = [python38 python2];
  linter = [python38Packages.flake8 python38Packages.mccabe];
  formatter = [black];
};
in {
  home.packages = lib.flatten (builtins.attrValues lang);
}
