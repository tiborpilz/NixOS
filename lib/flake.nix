{ lib, inputs, ... }:
{
  mkApp = package:
    inputs.flake-utils.lib.mkApp {
      drv = package;
      exePath = "/bin/${package.meta.mainProgram or package.pname or package.name}";
    };
}
