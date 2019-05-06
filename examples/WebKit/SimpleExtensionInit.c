/*
 * This is a small wrapper for the extension initializer, making sure
 * that we start the Haskell runtime.
 */
#include <webkit2/webkit-web-extension.h>

#include <Rts.h>

/* Initializer for the extension on the Haskell side */
void initialize_simple_web_extension_with_user_data
                    (WebKitWebExtension *extension,
                     GVariant *user_data);

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data (WebKitWebExtension *extension,
                                                GVariant *user_data)
{
  /* Make sure that we run with the threaded runtime */
  int argc = 3;
  char *argv[] = { "-threaded", "+RTS", "-N", NULL };
  char **pargv = argv;

  /* Initialize Haskell runtime */
  hs_init_with_rtsopts(&argc, &pargv);

  /* Call the extension entry point on the Haskell side */
  initialize_simple_web_extension_with_user_data(extension, user_data);
}
