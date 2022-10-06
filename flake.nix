{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {self, ...}@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = inputs.nixpkgs.legacyPackages."${system}";
      in {
      packages.svg-filter =
        pkgs.haskellPackages.callCabal2nix "svg-filter" ./. {};
      packages.default = self.packages."${system}".svg-filter;
      apps.svg-filter = "${self.packages."${system}".svg-filter}/bin/svg-filter";
    });
}
