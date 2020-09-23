{ mkDerivation, base, gi-cairo-render, sdl2, stdenv }:
mkDerivation {
  pname = "gi-cairo-render-sdl";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gi-cairo-render sdl2 ];
  homepage = "https://github.com/cohomology/gi-cairo-render";
  description = "GI friendly Binding to the Cairo library";
  license = stdenv.lib.licenses.bsd3;
}
