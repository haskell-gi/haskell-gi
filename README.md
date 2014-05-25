haskell-gi
==========

Haskell bindings for GObject Introspection, based on the code in
http://git.rhydd.org/?p=haskell-gi;a=summary
and
https://gitorious.org/haskell-gi
and some portions of [gtk2hs](http://projects.haskell.org/gtk2hs/).

This version adds support for:
* Casting between GObject types.
* Conecting callbacks to signals.
* Reference counting for GObjects.
* Automatic conversion between array arguments and Haskell types.
* Proper ownership transfer of function arguments.
* Support for GObject properties using the gtk2hs notation.
* Callback arguments.

See `test/testGtk.hs` for a working usage example.
