{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.hlint
  ];
}
