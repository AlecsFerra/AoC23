{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskellPackages.ghcWithPackages (p: [
        p.split_0_2_4 
        p.memoize
        p.vector
      ]);
in
pkgs.mkShell {
  packages = [
    hs
  ];
}
