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

## Overrides

The Haskell function prototype is generated out of the introspection data held in the `.gir` files,
which are usually found in `/usr/share/gir-1.0`. It is a common problem that some `nullable`
annotations are missing here. This flag indicates that the value can be `NULL`, and so the
corresponding Haskell type should be a `Maybe`. Also, developers using your binding may build it on
older versions of the library where the  reflection data contains more errors.

Therefore each binding can have an overrides file which specifies extra `nullable` flags for the
library. There are two steps to generating this file.

1. Run the `Nullable.xslt` script.

2. Review the generated Haddock documents for inconsistencies.

For step 1, you can generate the overrides to match the version on your own computer with the
following command:

```
$ xsltproc Nullable.xslt <path-to-gir-file>
```

Paste the output into the `.overrides` file underneath a comment saying how it was generated.in your
system.

For step 2, look through your Haddock documentation for things like like "returns `Nothing` if the
item is not found". If the corresponding type is not a `Maybe` then you need to add an extra
override to flag the type as `nullable`. You should also report this as a bug to the authors of the
library you are binding.
