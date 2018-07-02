{ mkDerivation, base, random, vector, primitive, stdenv }:
mkDerivation {
  pname = "pure-random-pcg";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base random vector primitive ];
  homepage = "github.com/grumply/pure-random-pcg";
  license = stdenv.lib.licenses.bsd3;
}
