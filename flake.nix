{
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghc = pkgs.haskellPackages.ghcWithHoogle (ps: with ps;
                [ criterion text text-replace parsec HUnit vector array memoize ]);
      in {
        devShells.default = pkgs.mkShell {
          packages = [ ghc pkgs.haskell-language-server ];
        };
      });
}
