# haskell-gi

Generate Haskell bindings for GObject Introspection capable libraries.

## Using the generated bindings

The most recent versions of the generated bindings are available from hackage. To install, start by making sure that you have a recent (1.24 or later) version of `cabal-install`:
```sh
$ cabal install cabal-install
$ cabal --version
cabal-install version 1.24.0.0
compiled using version 1.24.0.0 of the Cabal library 
```

Then install the bindings you need. For instance, for the gtk+ bindings you can do:
```sh
$ cabal install gi-gtk
```
(Note: you may need to run this command twice, due to [a bug in cabal](https://github.com/haskell/cabal/issues/3436)).

That's it! Here is an example "Hello World" program:
```haskell
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

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
```
$ ghc -o hello hello.hs
$ ./hello
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

[![Build Status](https://travis-ci.org/haskell-gi/haskell-gi.svg?branch=master)](https://travis-ci.org/haskell-gi/haskell-gi) [![Join the chat at https://gitter.im/haskell-gi/haskell-gi](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/haskell-gi/haskell-gi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
