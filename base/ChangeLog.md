### 0.20.1

+ Add Data.GI.Base.CallStack, abstracting (and backporting to the
extent possible) the `HasCallStack` constraint present in newer
GHCs. Using this, we now include callstacks pervasively in the
generated code.

+ Improve the `WrappedPtr` implementation.

+ Deprecate `nulltoNothing`, it is better to simply fix the
overrides when necessary.

+ Make the semantics of GObject ownership transfer closer to those used by the Python bindings.
