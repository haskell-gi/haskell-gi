### 2.0.19

+ Update to haskell-gi(-base) 0.22

### 2.0.18

+ Update stack version to 12.10

### 2.0.17

+ Annotate properly the return argument of [`threadSelf`](https://hackage.haskell.org/package/gi-glib/docs/GI-GLib-Structs-Thread.html#v:threadSelf), fixes [issue 173](https://github.com/haskell-gi/haskell-gi/issues/173).

### 2.0.16

+ Remove enable-overloading flags, and use instead explicit CPP checks for 'haskell-gi-overloading-1.0', see [how to disable overloading](https://github.com/haskell-gi/haskell-gi/wiki/Overloading\#disabling-overloading).

### 2.0.15

+ [timeValToIso8601](https://hackage.haskell.org/package/gi-glib/docs/GI-GLib-Structs-TimeVal.html#v:timeValToIso8601) has a nullable return value.
