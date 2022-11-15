{ lib, pkgs, inputs, ... }:
with lib;

{
  config = {
    home.packages = [
      inputs.devenv
    ];
  };
}
