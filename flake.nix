{
  description = "Functional programming library";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        rb-vector = pkgs.sbcl.buildASDFSystem {
          pname = "rb-vector";
          version = "unspecified";
          src = pkgs.fetchFromGitHub {
            owner = "uthar";
            repo = "rb-vector";
            rev = "9b00212d7f44ff14f519e0a69d99529d7897b2be";
            hash = "sha256-YAXtt+7tVlaVZ6YO4h5oFaCz6O/CABDARSgtOmRJnls=";
          };
        };
        sbcl = pkgs.sbcl.withPackages (ps: with ps; [
          alexandria
          lparallel
          cl-hamt
          rb-vector
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ sbcl ];
        };
      }
    );
}
