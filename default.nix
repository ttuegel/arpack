{ pkgs ? (import ./nixpkgs {}) }:
let
  inherit (pkgs) haskellPackages;
in
haskellPackages.cabal.mkDerivation (self: {
  pname = "arpack";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    concurrentExtra
    controlMonadLoop
    dataDefault
    storableComplex
    transformers
    vector
    vectorAlgorithms
  ];
  buildTools = with haskellPackages; [ cabalInstall_1_18_0_2 ];
  pkgconfigDepends = [ pkgs.arpack ];
  enableSplitObjs = false;
})
