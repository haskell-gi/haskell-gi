# haskell-gi-examples
haskell-gi introductory examples inspired by [GTK+ documentation](https://developer.gnome.org/gtk3/stable).

## Examples implemented (for now!)
1. __Basic:__ [Basic example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
2. __Hello world:__ [Hello World example of GTK+ documents](https://developer.gnome.org/gtk3/stable/gtk-getting-started.html).
3. __Packed widgets:__ [Packing example of GTK+ documents](https://developer.gnome.org/gtk3/3.12/ch01s02.html).


## How to compile and run examples
As the author is familiar only with Haskell stack, the examples are built and run by this tool (any suggestions are welcome).

The examples compiled and tested only on Windows 10. So on other environments some tweaks may be needed.

First of all you should not compile with ghc 8.2.* as it is subject to the overload bug. See [haskell-gi documentation](https://github.com/haskell-gi/haskell-gi).

1. Install GTK+ in the MSYS2 environment of the stack. To do this use stack exec -- pacman ... commands in the [GTK+ windows installation guide](https://www.gtk.org/download/windows.php) (excluding MSYS2 installation step).
2. Setup `PKG_CONFIG_PATH` and `XDG_DATA_DIRS` environment variables on Windows.
3. Be sure `\mingw64\bin` and `\usr\bin` are in path when compiling with stack.  
3. Compile the examples with stack.
4. Run the examples with stack exec
 
