# haskell-gi-examples
haskell-gi introductory examples inspired by [GTK+ documentation](https://developer.gnome.org/gtk3/stable).

## Examples implemented (for now!)
1. __Basic:__ [Basic example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
2. __Hello world:__ [Hello World example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
2. __Attribute usage:__ As per the [gi-base documentation](https://hackage.haskell.org/package/haskell-gi-base-0.21.0/docs/Data-GI-Base.html#t:AttrOp) there are several ways to assign to the object properties for convenience:
    
    1. __Normal value assignment:__ Bare constants or return values from full function applications can be assigned to the properties by `(:=)` operator. This operator can be used in `new` object construction.
    2. __Assignment of the values encapsulated by IO monad:__ `(:=>)` operator can assign the values wrapped in `IO`. This operator can also be used in `new` object construction.
    3. __Function application:__ Functions can be used to calculate property values using `(:~)` operator. This function is provided by the current value of the property in its parameter (be aware that if the return type of `get` function for the property is `Maybe` then the value is wrapped in `Maybe`) and return value is assigned as the new value. This operator **can not** be used in `new` object construction.
    4. __Monadic function application:__ Results of functions returning `IO` wrapped values can be assigned by `(:~>)` operator. The current value of the property is passed in sole parameter of the function (note the type may be `Maybe a` depending on the get result for the same property). This operator **can not** be used in `new` object construction.

    This example is a contrieved program that shows all these property assignment operators. Just read the comments in the `appActivate` function. 

3. __Packed widgets:__ [Packing example of GTK+ documents](https://developer.gnome.org/gtk3/3.12/ch01s02.html).
4. __Builder:__ [Packing buttons with GtkBuilder example of GTK+ documents](https://developer.gnome.org/gtk3/stable/ch01s03.html).

    In this example we should get the .ui path from the command arguments or construct a path to the same directory of the executable.
    You should either copy `builder.ui` file to the same directory of the executable, or give the full path to the `builder.ui` as the only argument to the program.
5. __Search Bar:__ [search-bar.c example in GTK repository](https://gitlab.gnome.org/GNOME/gtk/blob/master/examples/search-bar.c)

    Although only `window` and `searchBar` `#show`ed in the original code but this results in a blank and unresponsive window. `#show`ing all components individually or `#showAll` of them at once at the end of the function solves the problem. I don't know whether this is a haskell-gi or the original code problem.
6. __Action Namespace:__ [actuion-namespace.c example in GTK repository](https://gitlab.gnome.org/GNOME/gtk/blob/master/examples/action-namespace.c)

    This example shows how menu items belonging to different namespaces can be enabled/disabled together. If the commented out line compiled then items in "win" namespace are enabled or else stay disabled.

    This example gives critical messages if linked with gi-gio older than version 2.0.18. Although marked critical, these messages are harmless.

## How to compile and run examples
As the author is familiar only with Haskell stack, the examples are built and run by this tool (any suggestions are welcome).

The examples compiled and tested only on Windows 10. So on other environments some tweaks may be needed.

First of all you should not compile with ghc 8.2.* as it is subject to the overload bug. See [haskell-gi documentation](https://github.com/haskell-gi/haskell-gi).

1. Install GTK+ in the MSYS2 environment of the stack. To do this use stack exec -- pacman ... commands in the [GTK+ windows installation guide](https://www.gtk.org/download/windows.php) (excluding MSYS2 installation step).
2. Setup `PKG_CONFIG_PATH` and `XDG_DATA_DIRS` environment variables on Windows.
3. Be sure `\mingw64\bin` and `\usr\bin` are in path when compiling with stack.  
3. Compile the examples with stack.
4. Run the examples with stack exec
 
