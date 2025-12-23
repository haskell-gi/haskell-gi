{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.gtk4
    pkgs.gtk4.dev
    pkgs.pcre2
    pkgs.pcre2.dev
    pkgs.gtksourceview5
    pkgs.gtksourceview5.dev
    pkgs.gobject-introspection
    pkgs.gobject-introspection.dev
    pkgs.expat
    pkgs.expat.dev
    pkgs.cairo
    pkgs.cairo.dev
    pkgs.glib
    pkgs.glib.dev
    pkgs.libsysprof-capture
    pkgs.libxdmcp
    pkgs.sdl3
    pkgs.sdl3.dev
    pkgs.util-linux
    pkgs.libselinux
    pkgs.libselinux.dev
    pkgs.libsepol
    pkgs.libsepol.dev
    pkgs.fribidi
    pkgs.fribidi.dev
    pkgs.libdeflate
    pkgs.libthai
    pkgs.libthai.dev
    pkgs.lerc
    pkgs.lerc.dev
    pkgs.libdatrie
    pkgs.libdatrie.dev
    pkgs.xz
    pkgs.xz.dev
    pkgs.zstd
    pkgs.zstd.dev
    pkgs.libwebp
    pkgs.libxml2
    pkgs.libxml2.dev
  ];
  nativeBuildInputs = [
    pkgs.pkg-config
    pkgs.haskell.compiler.ghc96
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}

