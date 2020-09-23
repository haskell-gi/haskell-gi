with (import ./nix/nixpkgs.nix);
let
  myHaskellPackages =  haskellPackages.extend (self: super: {
    gi-cairo-render = self.callPackage ./gi-cairo-render/default.nix { };
    gi-cairo-connector =  self.callPackage ./gi-cairo-connector/default.nix { };
    gi-gtk = pkgs.haskell.lib.overrideCabal (self.callHackageDirect {
      pkg = "gi-gtk";
      ver = "3.0.36";
      sha256 = "17i2398yr3hg7ycja534j5pwr31jiakkqqvpwqmvfq84zpmsvnk5"; } { }) (drv: {
      libraryPkgconfigDepends = [ pkgs.gtk3 ];
    });
});
in {
  sdl = myHaskellPackages.callPackage ./examples/sdl/default.nix { };
  clock = myHaskellPackages.callPackage ./examples/clock/default.nix { };
  hlabyrinth = myHaskellPackages.callPackage ./examples/hlabyrinth/default.nix { };
}
