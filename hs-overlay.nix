{ pkgs }:

let
hsLib = pkgs.haskell.lib;
src-xmlbf = builtins.fetchGit {
  url = "https://gitlab.com/k0001/xmlbf.git";
  rev = "79fa04463322ac1e1b59a1bf4b467f4ddea48d19";
};

in
# This expression can be used as a Haskell package set `packageSetConfig`:
pkgs.lib.composeExtensions
  (import "${src-xmlbf}/hs-overlay.nix" { inherit pkgs; })
  (self: super: {
     safe-money = super.callPackage safe-money/pkg.nix {};
     safe-money-aeson = super.callPackage safe-money-aeson/pkg.nix {};
     safe-money-cereal = super.callPackage safe-money-cereal/pkg.nix {};
     safe-money-serialise = super.callPackage safe-money-serialise/pkg.nix {};
     safe-money-store = super.callPackage safe-money-store/pkg.nix {};
     safe-money-xmlbf = super.callPackage safe-money-xmlbf/pkg.nix {};

     _shell = self.shellFor {
       withHoogle = false; # hoogle dependencies don't compile
       packages = p: [
         p.safe-money
         p.safe-money-aeson
         p.safe-money-store
         p.safe-money-xmlbf
         p.safe-money-cereal
         p.safe-money-serialise
       ];
     };
  })
