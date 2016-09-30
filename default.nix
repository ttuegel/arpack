{ nixpkgs ? import <nixpkgs> {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskell.packages.ghc801.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });
    };
  };
  filterSource = drv: pred:
    lib.overrideCabal drv
    (args: args // { src = builtins.filterSource pred args.src; });
  omitDirs =
    let
      omitted = [ ".git" "dist" ];
    in drv:
    filterSource drv
    (path: type:
      type != "directory" || !(stdenv.lib.elem (baseNameOf path) omitted));
  arpackCompat = arpack.override { openblas = openblasCompat; };
in
omitDirs (haskellPackages.callPackage ./arpack.nix { arpack = arpackCompat; })
