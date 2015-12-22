{ mkDerivation, arpack, base, concurrent-extra, containers
, control-monad-loop, data-default, hmatrix, ieee754, stdenv
, storable-complex, tasty, tasty-quickcheck, transformers, vector
, vector-algorithms
}:
mkDerivation {
  pname = "arpack";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base concurrent-extra containers control-monad-loop data-default
    storable-complex transformers vector vector-algorithms
  ];
  libraryPkgconfigDepends = [ arpack ];
  testHaskellDepends = [
    base hmatrix ieee754 tasty tasty-quickcheck vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
