# haskell-gi-examples
haskell-gi introductory examples inspired by [GTK+ documentation](https://developer.gnome.org/gtk3/stable).

## Examples implemented
1. __Basic:__ [Basic example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
2. __Hello world:__ [Hello World example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
2. __Attribute usage:__ As per the [haskell-gi-base documentation](https://hackage.haskell.org/package/haskell-gi-base-0.21.0/docs/Data-GI-Base.html#t:AttrOp) there are several ways to assign to the object properties for convenience:
    
    1. __Normal value assignment:__ Bare constants or return values from full function applications can be assigned to the properties by `(:=)` operator. This operator can be used in `new` object construction.
    2. __Assignment of the values encapsulated by IO monad:__ `(:=>)` operator can assign the values wrapped in `IO`. This operator can also be used in `new` object construction.
    3. __Function application:__ Functions can be used to calculate property values using `(:~)` operator. This function is provided by the current value of the property in its parameter (be aware that if the return type of `get` function for the property is `Maybe` then the value is wrapped in `Maybe`) and return value is assigned as the new value. This operator **can not** be used in `new` object construction.
    4. __Monadic function application:__ Results of functions returning `IO` wrapped values can be assigned by `(:~>)` operator. The current value of the property is passed in sole parameter of the function (note the type may be `Maybe a` depending on the get result for the same property). This operator **can not** be used in `new` object construction.

    This example is a contrieved program that shows all these property assignment operators. Just read the comments in the `appActivate` function. 

3. __Packed widgets:__ [Packing example of GTK+ documents](https://developer.gnome.org/gtk3/3.12/ch01s02.html).
4. __Builder:__ [Packing buttons with GtkBuilder example of GTK+ documents](https://developer.gnome.org/gtk3/stable/ch01s03.html).

    In this example we get the .ui path from the command argument or, if no arguments are given, we try to find the .ui file in the current directory.

5. __Search Bar:__ [search-bar.c example in GTK repository](https://gitlab.gnome.org/GNOME/gtk/blob/master/examples/search-bar.c)

    Although only `window` and `searchBar` `#show`ed in the original code but this results in a blank and unresponsive window. `#show`ing all components individually or `#showAll` of them at once at the end of the function solves the problem. I don't know whether this is a haskell-gi or the original code problem.
6. __Action Namespace:__ [action-namespace.c example in GTK repository](https://gitlab.gnome.org/GNOME/gtk/blob/master/examples/action-namespace.c)

    This example shows how menu items belonging to different namespaces can be enabled/disabled together. If the commented out line compiled then items in "win" namespace are enabled or else stay disabled.

    This example gives critical messages if linked with gi-gio older than version 2.0.18. Although marked critical, these messages are harmless.
7. __Resource Bundle:__ This simple example is designed to show how to bundle various resources together the GTK+ program uses. It shows some images which are loaded from the resource bundle, as well as the UI definition and logo images for the application.

    __Attention!__ Please use `stack >= 1.7.0.1` version when building this example.

    There are two types of resource bundles:
    * __Internal resources:__ This type of resources are embedded into the executable. So resources are ready as soon as the program loaded. In this example this type of resources are default.
    * __External resources:__ This resource bundles are in separate files from the executable. Some times this is necessary because more than one program may share same resources. To prepare for this type of resource bundles, you should take these steps before program compilation and resource bundling:
        * __Step 1:__ Find `resource-bundle.cmd` (or the equivalent for your OS). Comment out second line and delete `@rem` at the start of the 6th line.
        * __Step 2:__ Delete comment markers of lines 8-9, 19-22 and 30-31 in `resource-bundle.hs`. 
        * __Step 3:__ Comment out lines 75-76 in `package.yaml`. Be sure to delete `.cabal` files (the backup of this file is `introductory-examples.cabal-save` if you want to return back to original). If you are using `Cabal` run `stack build --dry-run` to create the `.cabal` file.
    
    To run the program,
    1. You should generate the resource bundle that includes various resources the program needs. 
    
        For this refer to `resource-bundle.cmd` which is a Windows `CMD` batch file. Run this file if you are using Windows. If not the command in this file should be usable on all the OSes GTK+ runs.

        Be sure the the current directory is same with which the batch file resides when running command.
    2. * If you want embedded resources, compile the program.
        * If you want external resource bundle, once generated, copy resulted `resource-bundle.gresource` file to the directory where executable resides.

## How to compile and run examples
As the author is familiar only with Haskell stack, the examples are built and run by this tool (any suggestions are welcome).

The examples compiled and tested only on Windows 10. So on other environments some tweaks may be needed.

First of all you should not compile with ghc 8.2.* as it is subject to the overload bug. See [haskell-gi documentation](https://github.com/haskell-gi/haskell-gi).

1. Install GTK+ in the MSYS2 environment of the stack. To do this use stack exec -- pacman ... commands in the [GTK+ windows installation guide](https://www.gtk.org/download/windows.php) (excluding MSYS2 installation step).
2. Setup `PKG_CONFIG_PATH` and `XDG_DATA_DIRS` environment variables on Windows.
3. Be sure `\mingw64\bin` and `\usr\bin` are in path when compiling with stack.  
3. Compile the examples with stack.
4. Run the examples with stack exec
