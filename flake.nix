{
  description = "td-types";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgsForGraal.url = "github:nixos/nixpkgs?rev=1939434b4ae04cb855edec936573c778a9ddeab0";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgsForGraal,
      ...
    }@inputs:
    inputs.utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
      ]
      (
        system:
        let
          allPkgs = import ./pkgs.nix nixpkgs nixpkgsForGraal system;
          pkgs = allPkgs.default;
        in
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              sbt
              metals
              ammonite
              bloop
              coursier
              jdk
              mill
              sbt
              scala-cli
              scalafmt
            ];
          };
        }
      );
}
