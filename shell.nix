with (import <nixpkgs> {}).pkgs;
let
  arpackCompat = arpack.override { openblas = openblasCompat; };
in
haskellPackages.callPackage ./. { arpack = arpackCompat; }
