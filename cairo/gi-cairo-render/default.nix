with (import ./../nix/nixpkgs.nix);
{ mkDerivation, array, base, bytestring, c2hs, cairo
, haskell-gi-base, mtl, stdenv, text, utf8-string
}:
mkDerivation {
  pname = "gi-cairo-render";
  version = "0.1.0";
  sha256 = "1b2qxfahs4w288i6w5m0rs74amnm3zp0mj2vsccf34q437yni2gx";
  libraryHaskellDepends = [
    array base bytestring haskell-gi-base mtl text utf8-string
  ];
  preCompileBuildDriver = ''
    PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
    setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
  '';
  libraryPkgconfigDepends = [ pkgs.cairo ];
  libraryToolDepends = [ pkgs.haskellPackages.c2hs ];
  homepage = "https://github.com/cohomology/gi-cairo-render";
  description = "GI friendly Binding to the Cairo library";
  license = stdenv.lib.licenses.bsd3;
}
