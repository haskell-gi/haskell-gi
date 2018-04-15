# gi-cairo-render
[![Linux Build Status](https://img.shields.io/travis/cohomology/gi-cairo-render/master.svg?label=Linux%20build)](https://travis-ci.org/cohomology/hlabyrinth) 
[![Haskell](https://img.shields.io/badge/language-haskell-blue.svg)](https://www.haskell.org)
 
## Introduction
This is a fork of the [cairo](https://github.com/gtk2hs/gtk2hs/tree/master/cairo) Haskell module for use in conjunction with 
[Haskell GI](https://github.com/haskell-gi/haskell-gi). It can also be used without Haskell GI with fewer dependencies than
the original module.

Originally, accessing GTK+ from within Haskell used [gtk2hs](https://github.com/gtk2hs/gtk2hs). These were hand written bindings, providing a higher abstraction over the underlying GTK API. The problems are, that changes inside GTK+ have to be mirrored in Haskell, which makes maintaining the module increasingly difficult. The [Haskell GI](https://github.com/haskell-gi/haskell-gi) module tries to simplify the maintanance by using auto generated bindings from [GObject introspection data](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection?action=show&redirect=GObjectIntrospection). 

There is, however, nearly no GObject introspection data for cairo, as cairo is not based on the GObject data model. So the corresponding [gi-cairo](https://hackage.haskell.org/package/gi-cairo) library, contains nearly no useful functions except creating a [`GI.Cairo.Context`](https://hackage.haskell.org/package/gi-cairo-1.0.15/docs/GI-Cairo-Structs-Context.html).

This library provides:
1. A library `gi-cairo-render`, which is a fork of the old [cairo library](https://hackage.haskell.org/package/cairo)  with the same API, but fewer dependencies. Especially it does not depend on `gtk2hs-buildtools`, but only on upstream [c2hs](https://github.com/haskell/c2hs). Therefore it is less likely to break when `cabal` or `ghc` are upgraded.
2. A library `gi-cairo-connector` which is used to provide the glue code to plug `gi-cairo-render` inside `gi-cairo`, i.e. to actually _call_ `gi-cairo-render` from GTK+.

