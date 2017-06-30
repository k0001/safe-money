{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? builtins.fetchTarball
    https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.03.tar.gz
}:
let
pkgs = import nixpkgs {};
ghc802 = pkgs.haskell.packages.ghc802.override {
  packageSetConfig = self: super: {
    safe-money = self.callPackage (import ./pkg.nix) {};
    #store = self.callHackage "store" "0.4.3.1" {};
  };
};
ghcjsHEAD = pkgs.haskell.packages.ghcjsHEAD.override {
  packageSetConfig = self: super: {
    safe-money = self.callPackage (import ./pkg.nix) {};
  };
};

in {
  ghc802_safe-money = ghc802.safe-money;
  # upstream pkgset currently broken
  # ghcjsHEAD_safe-money = ghcjsHEAD.safe-money;
}
