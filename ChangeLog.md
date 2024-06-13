### 0.26.9

+ Add a workaround for a [GHC issue](https://gitlab.haskell.org/ghc/ghc/-/issues/23392) stopping parallel compilation in GHC >= 9.6.
+ Fix compilation issues regarding `time_t` and similar types in
  introspection data.

### 0.26.8

+ Add support for scope type "forever": see [this issue](https://github.com/haskell-gi/haskell-gi/issues/425).

### 0.26.7

+ Work around changing conventions on how to annotate user_data arguments in callbacks, see [this gobject introspection issue](https://gitlab.gnome.org/GNOME/gobject-introspection/-/issues/450) for background.

### 0.26.6

+ Work around changing conventions about what the `closure n` annotation means: many annotations appear on the callback, pointing to the user_data argument, but sometimes it also appears on the user_data argument, pointing to the callback. See [issue 407](https://github.com/haskell-gi/haskell-gi/issues/407) for a place where this becomes a problem.

### 0.26.5

+ Add a reference to ?self argument in signals. See [issue 408](https://github.com/haskell-gi/haskell-gi/issues/408) for the motivation.

### 0.26.0

+ Support for 'HasField' methods, which allows the syntax 'widget.show' or 'widget.add child' for invoking methods using the new [RecordDotSyntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst) in ghc 9.2.

+ And an implicit '?self' parameter for callbacks, for accessing the calling object. See [issue 346](https://github.com/haskell-gi/haskell-gi/issues/346) where this is necessary in practice in gtk4 to use event controllers.

+ Add an 'After' attribute to connect to signals after the default handler on constructors/setters using attribute syntax, similar to [On](https://hackage.haskell.org/package/haskell-gi-base/docs/Data-GI-Base-Attributes.html#t:AttrOp).

+ Add [resolveSignal](https://hackage.haskell.org/package/haskell-gi-base-0.26.0/docs/Data-GI-Base-Signals.html#v:resolveSignal) for showing what an overloaded signal resolves to: `resolveSignal button #notify` will output [GI.GObject.Objects.Object.Object::notify](https://hackage.haskell.org/package/gi-gobject-2.0.27/docs/GI-GObject-Objects-Object.html#g:signal:notify).

### 0.25.0

+ Support non-GObject object attributes.

+ Support for ghc 9.0.1.

+ Improvements in the generated Haddocks.

+ Remove the command line version of the bindings generator.

+ Remove support for non-IsLabel overloading.

+ Add [resolveMethod](https://hackage.haskell.org/package/haskell-gi-base-0.25.0/docs/Data-GI-Base-Overloading.html#v:resolveMethod) for showing what an overloaded method resolves to: `resolveMethod #show widget` will output `GI.Gtk.Objects.Widget.widgetShow`.

### 0.24.5

+ Fix an accidental double free for GValues, see [issue 320](https://github.com/haskell-gi/haskell-gi/issues/320).

+ Accept docsections in gir files, although they are currently ignored. See [issue 318](https://github.com/haskell-gi/haskell-gi/issues/318).

### 0.24.4

+ Relax bound on ansi-terminal.

### 0.24.3

+ Provide type init functions for GParamSpec types. This solves a puzzling linker error saying that the "intern" symbol could not be resolved, see [issue 297](https://github.com/haskell-gi/haskell-gi/issues/297) and [issue 298](https://github.com/haskell-gi/haskell-gi/issues/298).

### 0.24.2

+ Support for allocating GArrays of known size structs in caller-allocates arguments.

### 0.24.1

+ Add support for delete-attr override, to remove attributes.

+ Allow (but ignore) destroyers in scope async callbacks.

### 0.24.0

+ Added support for non-GObject objects

### 0.23.2

+ Fix a possible segfault in functions that return an out pointer to a dynamically allocated array, but do not initialize the array if it has zero size. See [#289](https://github.com/haskell-gi/haskell-gi/issues/289) for an example.

### 0.23.1

+ Check whether symbols exist in the dynamic library before trying to generate bindings for them, in order to avoid linker errors.

### 0.23.0

+ gobjectType now does not require a proxy argument, it needs to be used with TypeApplications instead.

+ Annotated signals are supported: `on widget (signal ::: "detail")`.

+ Safe coercions to parent types supported, with `asA`.

+ Support for GObject subclassing, and registering custom properties.

+ Use TypeApplications in `AttrInfo` implementation, and inherited methods implementation.

+ Add an allocating setting operator `(:&=)`.

+ Support for exporting class structs.

+ IsGValue instances for GObjects and boxed objects.

### 0.22.6

+ Fix generated IsX typeclasses for non-GObject interfaces.

### 0.22.5

+ Add support for inheriting overloading info.

### 0.22.4

+ Do not generate bindings for struct/union fields pointing to private/class structs, which we do not bind.

### 0.22.3

+ Sometimes struct fields marked as not introspectable contain invalid introspection info. We are lenient in these cases with parsing errors, and simply ignore the fields.

### 0.21.5

+ Add support for callback-valued properties.

### 0.21.4

+ Try to guess signedness of enums and flags on the C side, fixes [#184](https://github.com/haskell-gi/haskell-gi/issues/184).

### 0.21.3

+ Do not add nodes in overrides if a node with the same name already exists, fixes [#171](https://github.com/haskell-gi/haskell-gi/issues/171).

### 0.21.2

+ Do not free `Ptr Word8` types after performing the call to C,
since they only get passed along. Otherwise one could easily double free in functions such as [GdkPixbuf.pixbufNewFromData](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufNewFromData).

+ Fix a leak on optional `ScopeTypeAsync` callbacks.

### 0.20.4

+ Improve marshaling of array arguments with no specified size. This improves the generated bindings for various functions, for instance [`GObject.signalEmitv`](https://hackage.haskell.org/package/gi-gobject/docs/GI-GObject-Functions.html#v:signalEmitv).

+ Replace the enable-overloading flags with a explicit CPP check of the version of `haskell-gi-overloading` we are being compiled against, see [issue 124](https://github.com/haskell-gi/haskell-gi/issues/124) for the rationale.

### 0.20.3

+ Make the overloading code protected by a CPP conditional, depending on ENABLE_OVERLOADING being defined. See [issue 107](https://github.com/haskell-gi/haskell-gi/issues/107).

+ Wrap boxed structs/unions as transient [ManagedPtr](https://hackage.haskell.org/package/haskell-gi-base/docs/Data-GI-Base-BasicTypes.html#t:ManagedPtr)s in callbacks. This is needed to fix a number of issues, including [issue 96](https://github.com/haskell-gi/haskell-gi/issues/96) and [issue 97](https://github.com/haskell-gi/haskell-gi/issues/97).

### 0.20.2

+ Fixes for GHC 8.2.1.

### 0.20.1

+ gtk-doc parser and haddock generator: while by no means perfect,
now the autogenerated bindings come with some reasonable
autogenerated documentation.

+ Many bugfixes. A particularly important one is for
[issue 82](https://github.com/haskell-gi/haskell-gi/issues/82), which
made compilation of
[gi-glib](http://hackage.haskell.org/package/gi-glib) fail, for
the latest version of gobject-introspection.
