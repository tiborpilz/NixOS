{ lib, inputs, ... }:
{
  mkApp = package:
    inputs.flake-utils.lib.mkApp { drv = package; };
}
