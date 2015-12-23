{ mkDerivation, arpack, base, concurrent-extra, containers
, control-monad-loop, data-default, hmatrix, hspec, ieee754, stdenv
, storable-complex, transformers, vector, vector-algorithms
}:
mkDerivation {
  pname = "arpack";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base concurrent-extra containers control-monad-loop data-default
    hmatrix ieee754 storable-complex transformers vector
    vector-algorithms
  ];
  libraryPkgconfigDepends = [ arpack ];
  testHaskellDepends = [ base hmatrix hspec ];
  license = stdenv.lib.licenses.bsd3;
}
