{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskellPackages.ghcWithPackages (p: [
        p.split_0_2_4 
        p.memoize
        p.vector
        p.PSQueue
      ]);
in
pkgs.mkShell {
  packages = [
    hs
  ];
}
