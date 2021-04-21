# haskell-gi

Generate Haskell bindings for GObject Introspection capable libraries.

## Installation

To compile the bindings generated by `haskell-gi`, make sure that you have installed the necessary development packages for the libraries you are interested in. The following are examples for some common distributions. (If your distribution is not listed please send a pull request!)

### Fedora
```
sudo dnf install gobject-introspection-devel webkitgtk4-devel gtksourceview3-devel
```

### Debian / Ubuntu
```
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

### Arch Linux
```
sudo pacman -S gobject-introspection gobject-introspection-runtime gtksourceview3 webkit2gtk
```

### Mac OSX

Install [Homebrew](https://brew.sh/) and install GTK+ and GObject Introspection:

```
brew install gobject-introspection gtk+ gtk+3
```
Ensure the path to libffi (probably `/usr/local/opt/libffi/lib/pkgconfig`) is in the PKG_CONFIG_PATH environment variable.


### Windows

Please see [here](https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows) for detailed installation instructions in Windows.

## ⚠️ GHC 8.2.x ⚠️

Unfortunately there is [a bug](https://ghc.haskell.org/trac/ghc/ticket/14382) in `8.2.x` versions of GHC that makes it panic when compiling bindings generated by `haskell-gi`. Versions `8.0.x` and earlier of GHC are not affected by the bug, and version `8.4.1` has the bug fixed. The simplest workaround is to avoid using GHC `8.2.x` when using bindings generated by `haskell-gi`. Alternatively, [disabling overloading](https://github.com/haskell-gi/haskell-gi/wiki/Overloading#disabling-overloading) will avoid the bug.

## Using the generated bindings

The most recent versions of the generated bindings are available from hackage. To install, start by making sure that you have a recent (2.0 or later) version of `cabal-install`, for instance:
```sh
$ cabal install cabal-install
$ cabal --version
cabal-install version 2.4.1.0
compiled using version 2.4.1.0 of the Cabal library
```

Here is an example "Hello World" program:
```haskell
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{- cabal:
build-depends: base, haskell-gi-base, gi-gtk == 3.0.*
-}

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there" ]

  on win #destroy Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me" ]

  on button #clicked (set button [ #sensitive := False,
                                   #label := "Thanks for clicking me" ])

  #add win button

  #showAll win

  Gtk.main
```
This program uses the new `OverloadedLabels` extension in GHC 8.0, so make sure you have a recent enough version of GHC installed. To run this program, copy it to a file (`hello.hs`, say), and then
```sh
$ cabal v2-run hello.hs
```
For a more involved example, see for instance [this WebKit example](https://github.com/haskell-gi/haskell-gi/tree/master/examples). Further documentation can be found in [the Wiki](https://github.com/haskell-gi/haskell-gi/wiki).

## Translating from the C API to the `haskell-gi` generated API

The translation from the original C API to haskell-gi is fairly
straightforward: for method names simply remove the library prefix
(`gtk`, `gdk`, etc.), and convert to camelCase. I.e. `gtk_widget_show`
becomes
[`widgetShow`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetShow)
in the module `GI.Gtk` (provided by the `gi-gtk` package).

For properties, add the type of the object as a prefix: so the `sensitive` property of `GtkWidget` becomes [`widgetSensitive`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetSensitive) in `gi-gtk`. These can be set using the `new` syntax, as follows:

    b <- new Button [widgetSensitive := True]

or using `set` after having created the button

    b `set` [widgetSensitive := False]

Alternatively you can use [`setWidgetSensitive`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:setWidgetSensitive) and friends to set properties individually if you don't like the list syntax.

Finally, for signals you want to use the `onTypeSignalName` functions, for example [`onButtonClicked`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Button.html#v:onButtonClicked):

    onButtonClicked b $ do ...

This is the basic dictionary. Note that all the resulting symbols can be conveniently searched in [hoogle](http://hoogle.haskell.org).

There is also support for the `OverloadedLabels` extension in GHC 8.0 or higher. So the examples above can be shortened (by omitting the type that introduces the signal/property/method) to

    b <- new Button [#sensitive := True]
    on b #clicked $ do ...
    #show b

Hopefully this helps to get started! For any further questions there is a gitter channel that may be helpful at https://gitter.im/haskell-gi/haskell-gi.

##  Binding to new libraries

It should be rather easy to generate bindings to any library with `gobject-introspection` support, see the examples in the [bindings](https://github.com/haskell-gi/haskell-gi/tree/master/bindings) folder. Pull requests appreciated!

When adding new bindings, it may be necessary to augment the search paths used for `.gir` and `.typelib`. This can be done by setting the environment variables `HASKELL_GI_GIR_SEARCH_PATH` and `HASKELL_GI_TYPELIB_SEARCH_PATH` resepctively.

## Higher-Level Bindings

The bindings in `haskell-gi` aim for complete coverage of the bound APIs, but as a result they are imperative in flavour. For nicer, higher-level approaches based on these bindings, see:

* [gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative)
* [reactive-banana-gi-gtk](https://github.com/mr/reactive-banana-gi-gtk)

## Other Resources

* [Haskell at Work screencast: GTK+ Programming with Haskell](https://haskell-at-work.com/episodes/2018-11-13-gtk-programming-with-haskell.html)

---

[![Join the chat at https://gitter.im/haskell-gi/haskell-gi](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/haskell-gi/haskell-gi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) ![Linux CI](https://github.com/haskell-gi/haskell-gi/workflows/Linux%20CI/badge.svg)
