{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskellPackages.ghcWithPackages (p: [
        p.split_0_2_4 
        p.memoize
      ]);
in
pkgs.mkShell {
  packages = [
    hs
  ];
}
