with (import ./../nix/nixpkgs.nix);
{ mkDerivation, base, gi-cairo, gi-cairo-render, haskell-gi-base
, mtl, stdenv
}:
mkDerivation {
  pname = "gi-cairo-connector";
  version = "0.1.0";
  sha256 = "115iy6sd77aabzghdmfpn5w2zqqalrxgbs5i93z49y3vz4wsjiwf";
  libraryHaskellDepends = [
    base gi-cairo gi-cairo-render haskell-gi-base mtl
  ];
  libraryPkgconfigDepends = [ pkgs.cairo ];
  preCompileBuildDriver = ''
    PKG_CONFIG_PATH+=":${cairo}/lib/pkgconfig"
    setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
  '';
  homepage = "https://github.com/cohomology/gi-cairo-render";
  description = "GI friendly Binding to the Cairo library";
  license = stdenv.lib.licenses.lgpl21;
}
