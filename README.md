haskell-gi
==========

Haskell bindings for GObject Introspection, based on the code in
http://git.rhydd.org/?p=haskell-gi;a=summary
and
https://gitorious.org/haskell-gi
and some portions of [gtk2hs](http://projects.haskell.org/gtk2hs/).

This version adds support for:
* Type conversions.
* Conecting callbacks to signals.
* Reference counting for GObjects.
* Automatic conversion between G(S)List arguments and Haskell lists.

See `test/testGtk.hs` for a working usage example.
