@rem resource bundle command for windows
stack exec glib-compile-resources -- --sourcedir=. --generate --target=resources.c ./resource-bundle.gresource.xml

@rem External resource bundles Step 1:
@rem if you want a separate resource file from the executable the use below command instead of the one above
@rem stack exec glib-compile-resources -- --sourcedir=. --sourcedir=./image .\resource-bundle.gresource.xml