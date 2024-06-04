{
  description = "Rhine-koans presentation";

  nixConfig = {
    extra-substituters = [
      "https://rhine-koans.cachix.org"
    ];
    extra-trusted-public-keys = [
      "rhine-koans.cachix.org-1:cglDz0vWVge7HGENYsJRB6yU7+jZNXJyZ0Ud9Z0uW54="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

    nix-mkPandoc = {
      url = "github:chisui/nix-mkPandoc";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, nix-mkPandoc }:
    {
      packages.x86_64-linux = let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        default = import ./presentation { inherit pkgs nix-mkPandoc; };
      };
    };
}
