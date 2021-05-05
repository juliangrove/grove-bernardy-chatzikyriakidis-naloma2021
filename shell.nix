let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz);
in
{ pkgs ? import nixpkgs_source {
    inherit system;
  }
, system ? builtins.currentSystem
, nodejs ? pkgs."nodejs-4_x"
}:
let
  nodeEnv = import ../webppl/node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };
  nodePkgs = import ../webppl/node-packages.nix {
    inherit (pkgs) fetchurl fetchgit;
    inherit nodeEnv;
  };
  hp = pkgs.haskellPackages.override {
    overrides = self: super: {
      pretty-compact = self.callPackage ./pretty-compact.nix { };
      # typedflow = self.callPackage ./typedflow.nix {};

      gasp = self.callPackage
        ({ mkDerivation
         , base
         , binary
         , containers
         , fetchgit
         , mtl
         , QuickCheck
         , stdenv
         }:
          mkDerivation {
            pname = "gasp";
            version = "1.3.0.0";
            src = fetchgit {
              url = "https://github.com/jyp/gasp.git";
              sha256 = "0fd18x83sjxnqkbikb93rdl2vffmxh3835isiy1b7ilikbdpkmx5";
              rev = "c70466868c8436a759f0603815a22d33a4fe38cf";
              fetchSubmodules = true;
            };
            libraryHaskellDepends = [
              base
              binary
              containers
              mtl
              QuickCheck
            ];
            description = "A framework of algebraic classes";
            license = stdenv.lib.licenses.bsd3;
          })
        { };
    };
  };
  ghc = hp.ghcWithPackages (ps: with ps; ([
    cabal-install
    statistics
    QuickCheck
    gasp
  ]));
in
pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [
    # nodePkgs.webppl nodePkgs.npm nodejs nodePkgs.node-gyp
    ghc
    pkgs.gnuplot
  ];
  # export LOCALE_ARCHIVE=${stdenv.variable.LOCALE_ARCHIVE} # wrong invention
  shellHook = ''
    export LANG=C.UTF-8
    export LC_ALL=C.UTF-8
    # export LANG=en_US.UTF-8
    # export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}
