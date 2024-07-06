{ pkgs, lib, ... }: {
  nix = {
    # Helper util from nix-flakes-plus
    generateRegistryFromInputs = lib.mkDefault true;

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      system-features = [ "big-parallel" "kvm" "recursive-nix" ];
      trusted-users = [ "tibor" "root" ];
    };
    package = pkgs.nix;
  };
}
