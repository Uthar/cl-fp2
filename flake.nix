{
  description = "Functional programming library";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl = pkgs.sbcl.withPackages (ps: with ps; [
          alexandria
          lparallel
          cl-hamt
          tuple
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ sbcl ];
        };
      }
    );
}
