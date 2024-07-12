{
  description = "td-types";

  inputs = {
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs: inputs.utils.lib.eachSystem [
    "x86_64-linux" "aarch64-linux" "x86_64-darwin"
  ] (system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.default = pkgs.mkShell rec {
      packages = with pkgs; [
        pkgs.sbt

        pkgs.metals
      ];
    };
  });
}

