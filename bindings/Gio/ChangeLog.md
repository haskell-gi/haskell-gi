### 2.0.18

+ The `parameter` parameter for the [`activate`](https://hackage.haskell.org/package/gi-gio-2.0.18/docs/GI-Gio-Callbacks.html#t:ActionEntryActivateFieldCallback) callback of `ActionEntry` is nullable, but it is not marked as such in the introspection data.

### 2.0.17

+ The return value of [`applicationNew`](https://hackage.haskell.org/package/gi-gio/docs/GI-Gio-Objects-Application.html#v:applicationNew) can be null, but it is not marked as such in the introspection data, leading to a rather confusing error message (see [issue 142](https://github.com/haskell-gi/haskell-gi/issues/142) and [issue 143](https://github.com/haskell-gi/haskell-gi/issues/143)).

### 2.0.16

+ The return value of [`volumeGetMount`](https://hackage.haskell.org/package/gi-gio/docs/GI-Gio-Interfaces-Volume.html#v:volumeGetMount) is nullable, but was not marked as such.

### 2.0.15

+ Remove enable-overloading flags, and use instead explicit CPP checks for 'haskell-gi-overloading-1.0', see [how to disable overloading](https://github.com/haskell-gi/haskell-gi/wiki/Overloading\#disabling-overloading).

