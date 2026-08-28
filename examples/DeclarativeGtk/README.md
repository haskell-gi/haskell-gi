This is a test of the declarative-gtk library, adding a Model-View-Handler framework on top of gi-gtk.

Some quick notes:

+ [`mvh`](https://github.com/haskell-gi/haskell-gi/blob/5ecc721655e825c822af032f5371b04f62dbad90/declarative-gtk/Declarative/Gtk/ModelViewHandler.hs#L165) creates a Model-View-Handler component that you can plug into ordinary gi-gtk containers as a child.
+ [`widget`](https://github.com/haskell-gi/haskell-gi/blob/5ecc721655e825c822af032f5371b04f62dbad90/declarative-gtk/Declarative/Gtk.hs#L26) creates a "View" widget that depends on changeable values of the model (which we call [`DynVal`](https://github.com/haskell-gi/haskell-gi/blob/master/base/Data/GI/Base/DynVal.hs)s).
+ The operator [`:<~`](https://github.com/haskell-gi/haskell-gi/blob/5ecc721655e825c822af032f5371b04f62dbad90/base/Data/GI/Base/Attributes.hs#L519) in a view widget creates a directed binding from DynVal to a gobject property, so whenever the model changes the property changes. For example, this would make the text on a `GtkLabel` change whenever the model changes.
+ [`Bind`](https://github.com/haskell-gi/haskell-gi/blob/5ecc721655e825c822af032f5371b04f62dbad90/base/Data/GI/Base/Attributes.hs#L534) creates a bidirectional binding, so whenever the GObject property changes the model changes, and vice-versa.
+ Note that `DynVal`s have functor and applicative instances, so you can apply functions to `DynVal`s to obtain new `DynVal`s.
+ `emit` emits a message for the handler to handle.
+ [`<~`](https://github.com/haskell-gi/haskell-gi/blob/5ecc721655e825c822af032f5371b04f62dbad90/declarative-gtk/Declarative/Gtk/ModelViewHandler.hs#L292) in a handler modifies the model.
