{ pkgs }:

# This expression can be used as a Haskell package set `packageSetConfig`:
self: super: {
   safe-money = super.callPackage safe-money/pkg.nix {};
   safe-money-aeson = super.callPackage safe-money-aeson/pkg.nix {};
   safe-money-cereal = super.callPackage safe-money-cereal/pkg.nix {};
   safe-money-serialise = super.callPackage safe-money-serialise/pkg.nix {};
   safe-money-store = super.callPackage safe-money-store/pkg.nix {};
   safe-money-xmlbf = super.callPackage safe-money-xmlbf/pkg.nix {};

   _shell = self.shellFor {
     withHoogle = false; # hoogle dependencies don't compile
     buildInputs = [ self.cabal-install ];
     packages = p: [
       p.safe-money
       p.safe-money-aeson
       p.safe-money-store
       p.safe-money-xmlbf
       p.safe-money-cereal
       p.safe-money-serialise
     ];
   };
}
