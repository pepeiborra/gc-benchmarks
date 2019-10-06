# Inspired by https://mpickering.github.io/posts/2018-01-05-ghchead-nix.html
nixpkgs: nixpkgsSup:
with nixpkgs;
let
  patchRepo =
    fetchFromGitHub{
      owner = "hvr";
      repo = "head.hackage";
      rev = "3d067229c463ca2c1eb52bf353620f7ce9ebc265";
      sha256 = "1gq3c0racwmkjxfm67hg63c0akp1wfj5g855llif8q0kiivyqjap";
    };
    patchDir = "${patchRepo}/patches";
    patchScript = "${patchRepo}/scripts/overrides.nix";
    callNonMovingHaskellPackage = buildPackages.newScope {
      haskellLib = haskell.lib;
      overrides = haskell.packageOverrides;
    };
    localHackageOverrides = sel: sup:
        { haskell-src-exts = sel.callHackage "haskell-src-exts" "1.20.1" {};
          # fix the nixpkgs configuration for 8.9
          unordered-containers = sel.callHackage "unordered-containers" "0.2.10.0" {};
          # overriding mkDerivation leads to circular recursion for reasons unknown
          # mkDerivation = drv: sup.mkDerivation (drv // { jailbreak = true; doHaddock = false;});
        };
in
{ haskell = nixpkgsSup.haskell // {
    headHackagePatches = callPackage patchScript { patches = patchDir; };
    headHackageOverrides = callPackage haskell.headHackagePatches {};
    compiler = nixpkgsSup.haskell.compiler // {
      ghc891NonMoving = callPackage ./8.9.1.nonMoving.nix {
            bootPkgs = haskell.packages.ghc863Binary;
            inherit (buildPackages.python3Packages) sphinx;
            buildLlvmPackages = buildPackages.llvmPackages_7;
            llvmPackages = llvmPackages_7;
          };
      };
    packages = nixpkgsSup.haskell.packages // {
      ghc891NonMoving=
        (callNonMovingHaskellPackage "${<nixpkgs>}/pkgs/development/haskell-modules" {
          buildHaskellPackages = buildPackages.haskell.packages.ghc891NonMoving;
          ghc = buildPackages.haskell.compiler.ghc891NonMoving;
          compilerConfig = callNonMovingHaskellPackage "${<nixpkgs>}/pkgs/development/haskell-modules/configuration-ghc-8.8.x.nix" { };
        }).extend(lib.composeExtensions localHackageOverrides haskell.headHackageOverrides) ;
    };
  };
}
