# Bindings

## Generating the bindings

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

## Version numbers

The `girName` and `girVersion` fields in the `pkg.info` file refer to the name and version in the
`namespace` element of the `.gir` file.

Binding version numbers are `x.y.z` where `x.y` is the version of the GIR file you are creating
the binding for, and `z` is the revision of the binding. So the 25th revision of the bindings for
gtk3 are `3.0.25`, for example.
