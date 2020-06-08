{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, containers, data-default
      , errors, lens, mtl, pandoc-types, process, stdenv
      , system-filepath, text, transformers, xml-conduit, xml-lens
      }:
      mkDerivation {
        pname = "svg-filter";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          attoparsec base containers data-default errors lens mtl
          pandoc-types process system-filepath text transformers xml-conduit
          xml-lens
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    pandoc-types = haskellPackages.callHackage "pandoc-types" "1.20" {};
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
