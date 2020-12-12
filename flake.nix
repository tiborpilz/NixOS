{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/c2c0d57339c436744471b1337f362810ae8de340";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { home-manager, nixpkgs, ... }: {
    nixosConfigurations = {
      workyMcNixStation = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.tibor = import ./home-manager/home.nix;
          }
        ];
      };
    };
  };
}
