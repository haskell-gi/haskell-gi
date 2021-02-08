# Bindings

The `.cabal` files for the bindings in this directory can be generated
by running

```
cabal run genBuildInfo -- $(./PKGS.sh)
```

or if you want to generate a single `.cabal` file by passing in the directory
where the binding is, for example

```
cabal run genBuildInfo Gtk-4.0
```
