{ mkDerivation, base, random, vector, primitive, ghc-prim, stdenv }:
mkDerivation {
  pname = "pure-random-pcg";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base random vector primitive ghc-prim ];
  homepage = "github.com/grumply/pure-random-pcg";
  license = stdenv.lib.licenses.bsd3;
}
