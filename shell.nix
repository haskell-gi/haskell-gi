with (import (builtins.fetchTarball {
  "name" = "nixos-unstable-2020-09-22";
  "url"  = "https://github.com/nixos/nixpkgs/archive/1179840f9a88b8a548f4b11d1a03aa25a790c379.tar.gz";
  # `nix-prefetch-url --unpack <url>`
  "sha256" = "00jy37wj04bvh299xgal2iik2my9l0nq6cw50r1b2kdfrji8d563";
}) { config = { allowBroken = true; }; });
let
  myHaskellPackages = haskellPackages.extend (self: super: {
    gi-cairo-render = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
      pkg = "gi-cairo-render";
      ver = "0.1.0"; 
      sha256 = "1igw50n0r1lalnsn89ydmliww35rpmq0hhl5kyz2s6n149k2famb"; } { }) (drv: {
    libraryPkgconfigDepends = [ pkgs.cairo ];
    preCompileBuildDriver = ''
      PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
      setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
    '';
   });
   gi-cairo-connector = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
      pkg = "gi-cairo-connector";
      ver = "0.1.0"; 
      sha256 = "0pygmb85kzj463azr0mk6cvg7pivnwk9nkp5138i37ys91c4ri7h"; } { }) (drv: {
    libraryPkgconfigDepends = [ pkgs.cairo ];
    preCompileBuildDriver = ''
      PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
      setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
    ''; 
   });
   gi-gtk = pkgs.haskell.lib.overrideCabal (self.callHackageDirect { 
      pkg = "gi-gtk";
      ver = "3.0.36"; 
      sha256 = "17i2398yr3hg7ycja534j5pwr31jiakkqqvpwqmvfq84zpmsvnk5"; } { }) (drv: {
        libraryPkgconfigDepends = [ pkgs.gtk3 ];
   }); 
  });
  haskellDeps = ps: with ps; [
    gi-cairo
    gi-cairo-render
    gi-cairo-connector
    array
    base
    bytestring
    cereal
    directory
    errors
    filepath
    gi-gdk
    gi-glib
    gi-gtk
    gi-pango
    gi-pangocairo
    haskell-gi-base
    hxt-unicode
    mmorph
    mtl
    optparse-applicative
    pureMD5
    safecopy
    search-algorithms
    stm
    text
    transformers
  ];
  ghc = myHaskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    ghc
    haskellPackages.cabal-install
  ];
in
mkShell {
  buildInputs = [ 
    nixPackages    
  ];
}
