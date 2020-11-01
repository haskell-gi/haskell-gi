### 0.24.4

+ Add a workaround for old hsc2hs versions, so drop the constraint on hsc2hs.

### 0.24.3

+ Require hsc2hs version 0.68.6 or higher

### 0.24.2

+ Provide type init functions for GParamSpec types.

### 0.24.1

+ Support for allocating `GArray`s.

### 0.24.0

+ Support for non-GObject objects. As part of this work the GObject hierarchy has been slightly reworked. The main change is that 'gobjectType' has now become [glibType](https://hackage.haskell.org/package/haskell-gi-base-0.24.0/docs/Data-GI-Base-BasicTypes.html#v:glibType) (part of the [TypedObject](https://hackage.haskell.org/package/haskell-gi-base-0.24.0/docs/Data-GI-Base-BasicTypes.html#t:TypedObject) typeclass).

### 0.22.2

+ Reinstate the new' method.

### 0.22.1

+ Fix a memory allocation error in [GClosure](https://hackage.haskell.org/package/haskell-gi-base-0.22.0/docs/Data-GI-Base.html#t:GClosure) that could lead to crashes.

### 0.22.0

+ Require base >= 0.4.9 (GHC version >= 8.0), so that we can use TypeApplications.

+ Make [GClosure](https://hackage.haskell.org/package/haskell-gi-base-0.22.0/docs/Data-GI-Base.html#t:GClosure) a primitive type, and make it depend on a phantom parameter to increase type safety.

### 0.21.5

+ Add [releaseObject](https://hackage.haskell.org/package/haskell-gi-base-0.21.5/docs/Data-GI-Base-ManagedPtr.html#v:releaseObject), a function useful for manually releasing memory associated to GObjects.

### 0.21.4

+ Add support for callback-valued properties.

### 0.21.3

+ Fix a compilation error on Windows, see [issue 193](https://github.com/haskell-gi/haskell-gi/issues/193).

### 0.21.2

+ Export [newManagedPtr_](https://hackage.haskell.org/package/haskell-gi-base-0.21.2/docs/Data-GI-Base-ManagedPtr.html#v:newManagedPtr_).

### 0.21.1

+ Remove the `::=` and `::~` constructors in `AttrOp`, since they cannot really be used for anything, as they are pure functions.

### 0.21.0

+ New release to keep major version parity with the `haskell-gi` package, no changes otherwise.

### 0.20.8

+ Fix a bug which could lead to crashes when releasing boxed objects, see [issue #130](https://github.com/haskell-gi/haskell-gi/issues/130).

### 0.20.7

+ Fix a memory leak in doConstructGObject.

### 0.20.6

+ Use g_object_new_with_properties instead of g_object_newv in
GLib versions 2.54 or later, to avoid a deprecation warning.

### 0.20.5

+ Run object finalizers in the main loop. The reason is that for
some types the destructor is not thread safe, and assumes that it
is being run from the same thread as the thread that created the object,
which can lead to crashes when using the threaded runtime.

### 0.20.4

+ Better error diagnostics for [wrapObject](https://hackage.haskell.org/package/haskell-gi-base/docs/Data-GI-Base-ManagedPtr.html#v:wrapObject) and [newObject](https://hackage.haskell.org/package/haskell-gi-base/docs/Data-GI-Base-ManagedPtr.html#v:newObject).

### 0.20.3

+ Fixes for GHC 8.2.1 (and the corresponding `base-4.10.0`).

### 0.20.2

+ Fix fromGVariant for empty arrays, see [#91](https://github.com/haskell-gi/haskell-gi/issues/91) for details.

### 0.20.1

+ Add Data.GI.Base.CallStack, abstracting (and backporting to the
extent possible) the `HasCallStack` constraint present in newer
GHCs. Using this, we now include callstacks pervasively in the
generated code.

+ Improve the `WrappedPtr` implementation.

+ Deprecate `nulltoNothing`, it is better to simply fix the
overrides when necessary.

+ Make the semantics of GObject ownership transfer closer to those used by the Python bindings.
