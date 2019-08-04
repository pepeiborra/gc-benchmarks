let
  nixpkgs = fetchGit{
    name = "nixpkgs-08-03";
    url = https://github.com/NixOS/nixpkgs/;
    rev = "0fd978baf77e15c4eb9ba7a54111c340b0730b8f";
  };
  overlay = import ./nix/overlay.nix;
in with (import nixpkgs {overlays = [overlay];});
stdenv.mkDerivation {
  name = "gc-benchmarks";
  # outputs = [ "plot.svg" ];
  src = ./.;
  buildInputs = [
    haskell.compiler.ghc881NonMoving
    plotutils
    coreutils
  ];
}
