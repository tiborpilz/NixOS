{ inputs, ... }:
{
  # An ISO that *is* the system, as opposed to lib/installer.nix's mkIso, whose
  # media only installs some other host. The spec supplies every module, so what
  # boots is exactly what it describes -- see isos/amnesiac.
  mkLiveIso =
    { modules ? [ ]
    , system ? "x86_64-linux"
    , ...
    }:
    (inputs.nixpkgs.lib.nixosSystem {
      inherit system modules;
      specialArgs = { inherit inputs; };
    }).config.system.build.isoImage;
}
