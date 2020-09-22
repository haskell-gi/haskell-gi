with (import ./nix/nixpkgs.nix);
let
  myHaskellPackages = import ./nix/overrides.nix;
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
