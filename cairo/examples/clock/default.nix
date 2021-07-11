{ mkDerivation, base, gi-cairo, gi-cairo-connector, gi-cairo-render
, gi-gdk, gi-glib, gi-gtk, stdenv, text, time
}:
mkDerivation {
  pname = "gi-cairo-render-clock";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base gi-cairo gi-cairo-connector gi-cairo-render gi-gdk gi-glib
    gi-gtk text time
  ];
  homepage = "https://github.com/cohomology/gi-cairo-render";
  description = "Clock demo for cairo-render";
  license = stdenv.lib.licenses.bsd3;
}
