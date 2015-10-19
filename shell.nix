{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
(haskellPackages.callPackage (import ./.) {}).env

# See http://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages
