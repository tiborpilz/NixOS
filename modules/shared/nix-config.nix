{ pkgs, lib, ... }: {
  nix = {
    # Helper util from nix-flakes-plus
    generateRegistryFromInputs = lib.mkDefault true;

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      system-features = [ "big-parallel" "kvm" "recursive-nix" "nixos-test" ];
      trusted-users = [ "tibor" "root" ];
      trusted-substituters = [ "https://cache.nixos.org/" "https://tiborpilz.cachix.org/" ];
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org/"
        "https://tiborpilz.cachix.org/"
        "https://cache.garnix.io"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "tiborpilz.cachix.org-1:KyBjAXY8eblxntQ+OG13IjT+M222VxT+25yw1lqnQS4="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
      ];
    };
  };
}
