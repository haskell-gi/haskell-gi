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
For a more involved example, see for instance [this WebKit example](https://github.com/haskell-gi/haskell-gi/tree/master/examples).

##  Binding to new libraries

It should be rather easy to generate bindings to any library with `gobject-introspection` support, see the examples in the [bindings](https://github.com/haskell-gi/haskell-gi/tree/master/bindings) folder. Pull requests appreciated!

[![Join the chat at https://gitter.im/haskell-gi/haskell-gi](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/haskell-gi/haskell-gi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
