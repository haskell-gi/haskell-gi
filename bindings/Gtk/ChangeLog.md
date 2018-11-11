### 3.0.26

+ Make sure that the 'data' argument of [CellLayoutDataFunc](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Callbacks.html#t:CellLayoutDataFunc) is properly anotated as a closure argument.

### 3.0.25

+ Update stack version to 12.10

+ Add missing nullable return annotation to [treePathGetIndices](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Structs-TreePath.html#v:treePathGetIndices), see [issue 192](https://github.com/haskell-gi/haskell-gi/issues/192).

### 3.0.24

+ Mark the return value of [applicationGetActiveWindow](https://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Application.html#v:applicationGetActiveWindow) as nullable, see [issue 176](https://github.com/haskell-gi/haskell-gi/issues/176).

### 3.0.23

+ Add an override to make sure that [`Cairo.FontOptions`](https://hackage.haskell.org/package/gi-cairo/docs/GI-Cairo-Structs-FontOptions.html) is always treated as boxed.

### 3.0.22

+ The return value of [`applicationNew`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Application.html#v:applicationNew) can be null, but it is not marked as such in the introspection data, leading to a rather confusing error message (see [issue 142](https://github.com/haskell-gi/haskell-gi/issues/142) and [issue 143](https://github.com/haskell-gi/haskell-gi/issues/143)).

### 3.0.21

+ The return value of [`widgetDragDestFindTarget`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetDragDestFindTarget) is nullable, but was not marked as such.

### 3.0.20

+ Make the `events` parameter to [`widgetAddEvents`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetAddEvents) and [`widgetSetEvents`](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetSetEvents) a [`GdkEventMask`](https://hackage.haskell.org/package/gi-gdk/docs/GI-Gdk-Flags.html#t:EventMask), see [issue 136](https://github.com/haskell-gi/haskell-gi/issues/136).

### 3.0.19

+ Remove enable-overloading flags, and use instead explicit CPP checks for 'haskell-gi-overloading-1.0', see [how to disable overloading](https://github.com/haskell-gi/haskell-gi/wiki/Overloading\#disabling-overloading).

### 3.0.18

+ Fix introspection info for [buttonSetImage](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Button.html#v:buttonSetImage). Image parameter can be null.

### 3.0.15

+ Fix introspection info for [uIManagerGetAction](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetAction), [uIManagerGetWidget](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetWidget), [actionGroupGetAction](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetAction), [textMarkGetName](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetName) and [notebookGetTabLabel](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabLabel). In all these cases the function can return NULL, but it was not marked as such in the introspection data.

+ Fix introspection data for [builderAddCallbackSymbol](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Builder.html#v:builderAddCallbackSymbol). The `callbackSymbol` argument was marked as `(scope async)`, which meant that it was safe to free after the first invokation, but this was not correct, and leads to [crashes](https://github.com/haskell-gi/haskell-gi/issues/104).

### 3.0.14

Update lower version bound on haskell-gi (>= 0.20.1).

### 3.0.13

Update nullable overrides to match gtk+ 3.22.11.

### 3.0.12

Fix a mistake in the introspection data in [widgetGetParentwindow](https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Objects-Widget.html#v:widgetGetParentWindow), where the return value was not properly annotated as nullable. Fixes [#90](https://github.com/haskell-gi/haskell-gi/issues/90).
