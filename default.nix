{ mkDerivation, array, base, bytestring, cereal, directory, errors
, filepath, gi-cairo, gi-cairo-connector, gi-cairo-render, gi-gdk
, gi-glib, gi-gtk, gi-pango, gi-pangocairo, haskell-gi-base, hpack
, hxt-unicode, mmorph, mtl, optparse-applicative, pureMD5, safecopy
, search-algorithms, stdenv, stm, text, transformers
}:
mkDerivation {
  pname = "hlabyrinth";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    array base bytestring cereal directory errors filepath gi-cairo
    gi-cairo-connector gi-cairo-render gi-gdk gi-glib gi-gtk gi-pango
    gi-pangocairo haskell-gi-base hxt-unicode mmorph mtl
    optparse-applicative pureMD5 safecopy search-algorithms stm text
    transformers
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.gpl3;
}
