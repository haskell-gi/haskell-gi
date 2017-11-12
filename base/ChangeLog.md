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
