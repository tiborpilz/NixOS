{ lib, inputs, ... }:
{
  mkApp = package: {
    type = "app";
    program = "${package}/bin/${package.meta.mainProgram or package.pname or package.name}";
  };
}
