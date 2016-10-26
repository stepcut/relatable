{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, bookkeeper, container, ghc-prim, stdenv
      , text, type-level-sets, cabal-install
      }:
      mkDerivation {
        pname = "aql";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          acid-state base bookkeeper ghc-prim text cabal-install
        ];
        description = "strongly typed relational algebra";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
