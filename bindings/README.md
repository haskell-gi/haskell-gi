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

When adding new bindings, it may be necessary to augment the search
paths used for `.gir` and `.typelib` files. This can be done by
setting the environment variables `HASKELL_GI_GIR_SEARCH_PATH` and
`HASKELL_GI_TYPELIB_SEARCH_PATH` respectively.
