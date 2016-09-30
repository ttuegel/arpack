{ nixpkgs ? import <nixpkgs> {}, profiling ? false }:

(import ./default.nix { inherit nixpkgs profiling; }).env
