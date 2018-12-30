{ compiler ? "ghc863" }:

let
  pkgs = import <nixpkgs> {};
  dontCheck = pkgs.haskell.lib.dontCheck;
  haskellPkgs = pkgs.haskell.packages."${compiler}".extend (self: super: {
    broch-io = self.callPackage ./broch-io.nix {};
    broch = self.callPackage ./broch.nix {};
  });
in
  {
    broch-io = haskellPkgs.broch-io;
  }
