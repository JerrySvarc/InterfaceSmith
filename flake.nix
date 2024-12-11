{
  description = "An editor for creating UI elements based on concrete values.";

  # Flake inputs
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
         flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            dotnet-sdk_8
            nodejs_20
          ];

                };


    in
    {

        devShell = devShell;
    });
}