let
  nixpkgs = fetchGit{
    name = "nixpkgs-08-03";
    url = https://github.com/NixOS/nixpkgs/;
    rev = "0fd978baf77e15c4eb9ba7a54111c340b0730b8f";
  };
  overlay = import ./nix/overlay.nix;
in with (import nixpkgs {overlays = [overlay];});
let
  shakeGhc = haskellPackages.ghcWithPackages(p: [p.shake p.generic-deriving p.split p.diagrams p.diagrams-svg p.Chart p.Chart-diagrams]);
  shakeBuilder = stdenv.mkDerivation {
    name = "shakeBuilder";
    src = ./.;
    buildPhase ="${shakeGhc}/bin/ghc -O Shake.hs";
    installPhase = ''mkdir -p $out/bin && mv Shake $out/bin'';
  };
in stdenv.mkDerivation {
  name = "gc-benchmarks";
  # outputs = [ "plot.svg" ];
  src = ./.;
  buildPhase = "${shakeBuilder}/bin/Shake -j1 -V";
  installPhase = "${shakeBuilder}/bin/Shake install";
  buildInputs = [
    haskell.compiler.ghc881NonMoving
    plotutils
    coreutils
    pandoc
  ];
}
