{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
(haskellPackages.callPackage (import ./.) {}).env

